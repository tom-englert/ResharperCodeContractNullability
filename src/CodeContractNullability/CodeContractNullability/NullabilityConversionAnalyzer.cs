using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using CodeContractNullability.NullabilityAttributes;
using CodeContractNullability.SymbolAnalysis;
using CodeContractNullability.Utilities;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;

namespace CodeContractNullability
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class NullabilityConversionAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "CNUL";

        [NotNull]
        private readonly ImmutableDictionary<RuleKey, DiagnosticDescriptor> rules;

        [ItemNotNull]
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => rules.Values.ToImmutableArray();

        [NotNull]
        public ExtensionPoint<INullabilityAttributeProvider> NullabilityAttributeProvider { get; } =
            new ExtensionPoint<INullabilityAttributeProvider>(() => new CachingNullabilityAttributeProvider());

        public NullabilityConversionAnalyzer()
        {
            var ruleMap = new Dictionary<RuleKey, DiagnosticDescriptor>();

            foreach (SymbolAnalysisKind kind in Enum.GetValues(typeof (SymbolAnalysisKind)))
            {
                foreach (string baseAttributeName in new[] { "CanBeNullAttribute", "NotNullAttribute" })
                {
                    foreach (bool appliesToItem in new[] { true, false })
                    {
                        string attributeName = appliesToItem ? "Item" + baseAttributeName : baseAttributeName;

                        foreach (ConversionFixAction fixAction in Enum.GetValues(typeof (ConversionFixAction)))
                        {
                            var ruleKey = new RuleKey(kind, fixAction, baseAttributeName, appliesToItem);
                            DiagnosticDescriptor descriptor = fixAction == ConversionFixAction.RemoveAttribute
                                ? CreateRemoveRuleFor(kind.ToString().ToCamelCase(), attributeName)
                                : CreateConvertReferenceTypeRuleFor(kind.ToString().ToCamelCase(), attributeName);

                            ruleMap[ruleKey] = descriptor;
                        }
                    }
                }
            }

            rules = ruleMap.ToImmutableDictionary();
        }

        [NotNull]
        private DiagnosticDescriptor CreateRemoveRuleFor([NotNull] string memberTypeCamelCase,
            [NotNull] string attributeName)
        {
            string title = $"{attributeName} on {memberTypeCamelCase} can be removed.";
            string messageFormat = $"{attributeName} on {memberTypeCamelCase} '{{0}}' can be removed.";
            string description =
                $"This {memberTypeCamelCase} is annotated with {attributeName}; with nullable reference types enabled, the annotation can be removed.";

            return new DiagnosticDescriptor(DiagnosticId, title, messageFormat, "Nullability",
                DiagnosticSeverity.Warning, true, description, "TODO: HELP URL");
        }

        [NotNull]
        private DiagnosticDescriptor CreateConvertReferenceTypeRuleFor([NotNull] string memberTypeCamelCase,
            [NotNull] string attributeName)
        {
            string title = $"{attributeName} on {memberTypeCamelCase} can be converted to nullable reference type.";
            string messageFormat =
                $"{attributeName} on {memberTypeCamelCase} '{{0}}' can be converted to nullable reference type.";
            string description =
                $"This {memberTypeCamelCase} is annotated with {attributeName}; with nullable reference types enabled, it can be converted to a nullable reference type.";

            return new DiagnosticDescriptor(DiagnosticId, title, messageFormat, "Nullability",
                DiagnosticSeverity.Warning, true, description, "TODO: HELP URL");
        }

        public override void Initialize([NotNull] AnalysisContext context)
        {
            Guard.NotNull(context, nameof(context));

            // Possible scenarios (on field, property, method and parameter) for Symbol or ItemSymbol:
            // - [CanBeNull] on nullable value type =>  actions: remove attribute
            // - [CanBeNull] on reference type =>       actions: replace attribute with ? on (Item)Symbol
            // - [NotNull] on nullable value type =>    actions: remove attribute
            // - [NotNull] on reference type =>         actions: remove attribute

            // TODO: Something to think about: when replacing attribute in interface/base, that breaks derived class (because they no longer inherit the annotation)

            // TODO: Applying replace action on Unconstrained generics (without "where T : class" or "where T : struct") causes broken build. Is this a Roslyn bug?
            // Track: https://github.com/dotnet/roslyn/issues/9932
            // For the moment, should implement to skip offering Replace rule when it concerns an unconstrained T (to enable project/solution wide fixer).

            // TODO: Why do fixes appear twice in DogAgilityCompetitionManagement project?


            context.RegisterCompilationStartAction(StartAnalyzeCompilation);
        }

        private void StartAnalyzeCompilation([NotNull] CompilationStartAnalysisContext context)
        {
            Guard.NotNull(context, nameof(context));

            NullabilityAttributeSymbols nullSymbols =
                NullabilityAttributeProvider.GetCached().GetSymbols(context.Compilation, context.CancellationToken);
            if (nullSymbols == null)
            {
                // Nullability attributes not found; keep silent.
                return;
            }

            var typeCache = new FrameworkTypeCache(context.Compilation);

            context.RegisterSymbolAction(c => AnalyzeField(c, typeCache), SymbolKind.Field);
            context.RegisterSymbolAction(c => AnalyzeProperty(c, typeCache), SymbolKind.Property);
            context.RegisterSymbolAction(c => AnalyzeMethod(c, typeCache), SymbolKind.Method);
            context.RegisterSyntaxNodeAction(c => AnalyzeParameter(SyntaxToSymbolContext(c), typeCache),
                SyntaxKind.Parameter);
        }

        private void AnalyzeField(SymbolAnalysisContext context, [NotNull] FrameworkTypeCache typeCache)
        {
            var field = (IFieldSymbol) context.Symbol;
            AnalyzeSymbol(field, field.Type, context, SymbolAnalysisKind.Field, typeCache);
        }

        private void AnalyzeProperty(SymbolAnalysisContext context, [NotNull] FrameworkTypeCache typeCache)
        {
            var property = (IPropertySymbol) context.Symbol;
            AnalyzeSymbol(property, property.Type, context, SymbolAnalysisKind.Property, typeCache);
        }

        private void AnalyzeMethod(SymbolAnalysisContext context, [NotNull] FrameworkTypeCache typeCache)
        {
            var method = (IMethodSymbol) context.Symbol;
            AnalyzeSymbol(method, method.ReturnType, context, SymbolAnalysisKind.Method, typeCache);
        }

        private void AnalyzeParameter(SymbolAnalysisContext context, [NotNull] FrameworkTypeCache typeCache)
        {
            var parameter = (IParameterSymbol) context.Symbol;
            AnalyzeSymbol(parameter, parameter.Type, context, SymbolAnalysisKind.Parameter, typeCache);
        }

        private void AnalyzeSymbol([NotNull] ISymbol targetSymbol, [NotNull] ITypeSymbol symbolType,
            SymbolAnalysisContext context, SymbolAnalysisKind kind, [NotNull] FrameworkTypeCache typeCache)
        {
            foreach (string baseAttributeName in new[] { "CanBeNullAttribute", "NotNullAttribute" })
            {
                foreach (bool appliesToItem in new[] { true, false })
                {
                    string attributeName = appliesToItem ? "Item" + baseAttributeName : baseAttributeName;

                    var removeRuleKey = new RuleKey(kind, ConversionFixAction.RemoveAttribute, baseAttributeName,
                        appliesToItem);
                    DiagnosticDescriptor removeDescriptor = rules[removeRuleKey];

                    var replaceRuleKey = new RuleKey(kind, ConversionFixAction.ReplaceAttributeWithQuestionMark,
                        baseAttributeName, appliesToItem);
                    DiagnosticDescriptor replaceDescriptor = rules[replaceRuleKey];

                    ITypeSymbol currentSymbolType;
                    if (appliesToItem)
                    {
                        currentSymbolType = symbolType.TryGetItemTypeForSequenceOrCollection(typeCache) ??
                            symbolType.TryGetItemTypeForLazyOrGenericTask(typeCache);
                    }
                    else
                    {
                        currentSymbolType = symbolType;
                    }

                    if (currentSymbolType != null)
                    {
                        bool forCanBeNull = baseAttributeName == "CanBeNullAttribute";
                        ReportForAttribute(attributeName, context, targetSymbol, currentSymbolType, appliesToItem,
                            forCanBeNull, removeDescriptor, replaceDescriptor);
                    }
                }
            }
        }

        private void ReportForAttribute([NotNull] string attributeName, SymbolAnalysisContext context,
            [NotNull] ISymbol targetSymbol, [NotNull] ITypeSymbol symbolType, bool appliesToItem, bool forCanBeNull,
            [NotNull] DiagnosticDescriptor removeRule, [CanBeNull] DiagnosticDescriptor replaceRule)
        {
            AttributeData attribute =
                targetSymbol.GetAttributes().FirstOrDefault(attr => attr.AttributeClass.Name == attributeName);
            if (attribute != null)
            {
                ImmutableDictionary<string, string> fixTargetRemove =
                    new ConversionFixTarget(ConversionFixAction.RemoveAttribute, attributeName, appliesToItem)
                        .ToProperties();
                ImmutableDictionary<string, string> fixTargetReplace =
                    new ConversionFixTarget(ConversionFixAction.ReplaceAttributeWithQuestionMark, attributeName,
                        appliesToItem).ToProperties();

                if (symbolType.IsSystemNullableType())
                {
                    Diagnostic diagnostic = Diagnostic.Create(removeRule, context.Symbol.Locations[0], fixTargetRemove,
                        context.Symbol.Name);
                    context.ReportDiagnostic(diagnostic);
                }
                else if (!symbolType.IsValueType)
                {
                    if (forCanBeNull)
                    {
                        Diagnostic diagnostic = Diagnostic.Create(replaceRule, context.Symbol.Locations[0],
                            fixTargetReplace, context.Symbol.Name);
                        context.ReportDiagnostic(diagnostic);
                    }
                    else
                    {
                        Diagnostic diagnostic = Diagnostic.Create(removeRule, context.Symbol.Locations[0],
                            fixTargetRemove, context.Symbol.Name);
                        context.ReportDiagnostic(diagnostic);
                    }
                }
            }
        }

        private static SymbolAnalysisContext SyntaxToSymbolContext(SyntaxNodeAnalysisContext syntaxContext)
        {
            ISymbol symbol = syntaxContext.SemanticModel.GetDeclaredSymbol(syntaxContext.Node);
            return SyntaxToSymbolContext(syntaxContext, symbol);
        }

        private static SymbolAnalysisContext SyntaxToSymbolContext(SyntaxNodeAnalysisContext context,
            [NotNull] ISymbol symbol)
        {
            Guard.NotNull(symbol, nameof(symbol));

            return new SymbolAnalysisContext(symbol, context.SemanticModel.Compilation, context.Options,
                context.ReportDiagnostic, x => true, context.CancellationToken);
        }

        private struct RuleKey : IEquatable<RuleKey>
        {
            public SymbolAnalysisKind Kind { get; }
            public ConversionFixAction FixAction { get; }

            [NotNull]
            public string BaseAttributeName { get; }

            public bool AppliesToItem { get; }

            public RuleKey(SymbolAnalysisKind kind, ConversionFixAction fixAction, [NotNull] string baseAttributeName,
                bool appliesToItem)
            {
                Guard.NotNull(baseAttributeName, nameof(baseAttributeName));

                Kind = kind;
                FixAction = fixAction;
                BaseAttributeName = baseAttributeName;
                AppliesToItem = appliesToItem;
            }

            public bool Equals(RuleKey other)
            {
                return other.Kind == Kind && other.FixAction == FixAction &&
                    other.BaseAttributeName == BaseAttributeName && other.AppliesToItem == AppliesToItem;
            }

            public override bool Equals([CanBeNull] object obj)
            {
                return obj is RuleKey && Equals((RuleKey) obj);
            }

            public override int GetHashCode()
            {
                return Kind.GetHashCode() ^ FixAction.GetHashCode() ^ BaseAttributeName.GetHashCode() ^
                    AppliesToItem.GetHashCode();
            }

            public override string ToString()
            {
                var textBuilder = new StringBuilder();
                textBuilder.Append(FixAction == ConversionFixAction.RemoveAttribute ? "Remove " : "Replace ");
                textBuilder.Append(AppliesToItem ? "Item" + BaseAttributeName : BaseAttributeName);
                textBuilder.Append(" from ");
                textBuilder.Append(Kind.ToString().ToCamelCase());
                return textBuilder.ToString();
            }
        }

        private enum SymbolAnalysisKind
        {
            Field,
            Property,
            Method,
            Parameter
        }
    }

    public sealed class ConversionFixTarget
    {
        public ConversionFixAction FixAction { get; }

        [NotNull]
        public string AttributeName { get; }

        public bool AppliesToItem { get; }

        public ConversionFixTarget(ConversionFixAction fixAction, [NotNull] string attributeName, bool appliesToItem)
        {
            Guard.NotNull(attributeName, nameof(attributeName));

            FixAction = fixAction;
            AttributeName = attributeName;
            AppliesToItem = appliesToItem;
        }

        [NotNull]
        public ImmutableDictionary<string, string> ToProperties()
        {
            return
                ImmutableDictionary.Create<string, string>()
                    .Add("FixAction", FixAction.ToString())
                    .Add("AttributeName", AttributeName)
                    .Add("AppliesToItem", AppliesToItem.ToString());
        }

        [NotNull]
        public static ConversionFixTarget Parse([NotNull] ImmutableDictionary<string, string> properties)
        {
            var fixAction = (ConversionFixAction) Enum.Parse(typeof (ConversionFixAction), properties["FixAction"]);
            string attributeName = properties["AttributeName"];
            bool appliesToItem = bool.Parse(properties["AppliesToItem"]);

            return new ConversionFixTarget(fixAction, attributeName, appliesToItem);
        }
    }

    public enum ConversionFixAction
    {
        RemoveAttribute,
        ReplaceAttributeWithQuestionMark
    }
}