using System;
using System.Collections.Immutable;
using System.Linq;
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
        public ExtensionPoint<INullabilityAttributeProvider> NullabilityAttributeProvider { get; } =
            new ExtensionPoint<INullabilityAttributeProvider>(() => new CachingNullabilityAttributeProvider());

        [NotNull]
        private DiagnosticDescriptor CreateRemoveRuleFor([NotNull] string memberTypeCamelCase,
            [NotNull] string attributeName)
        {
            string title = $"{attributeName} attribute on {memberTypeCamelCase} can be removed.";
            string messageFormat = $"{attributeName} attribute on {memberTypeCamelCase} '{{0}}' can be removed.";
            string description =
                $"This {memberTypeCamelCase} is annotated with {attributeName} attribute; with nullable reference types enabled, the annotation can be removed.";

            return new DiagnosticDescriptor(DiagnosticId, title, messageFormat, "Nullability",
                DiagnosticSeverity.Warning, true, description, "TODO: HELP URL");
        }

        [NotNull]
        private DiagnosticDescriptor CreateConvertReferenceTypeRuleFor([NotNull] string memberTypeCamelCase,
            [NotNull] string attributeName)
        {
            string title =
                $"{attributeName} attribute on {memberTypeCamelCase} can be converted to nullable reference type.";
            string messageFormat =
                $"{attributeName} attribute on {memberTypeCamelCase} '{{0}}' can be converted to nullable reference type.";
            string description =
                $"This {memberTypeCamelCase} is annotated with {attributeName} attribute; with nullable reference types enabled, it can be converted to a nullable reference type.";

            return new DiagnosticDescriptor(DiagnosticId, title, messageFormat, "Nullability",
                DiagnosticSeverity.Warning, true, description, "TODO: HELP URL");
        }

        [NotNull]
        private readonly DiagnosticDescriptor canBeNullOnMethodThatReturnsNullableValueType;

        [NotNull]
        private readonly DiagnosticDescriptor canBeNullOnMethodThatReturnsReferenceType;

        [NotNull]
        private readonly DiagnosticDescriptor notNullOnMethod;

        [NotNull]
        private readonly DiagnosticDescriptor itemCanBeNullOnMethodThatReturnsNullableValueType;

        [NotNull]
        private readonly DiagnosticDescriptor itemCanBeNullOnMethodThatReturnsReferenceType;

        [NotNull]
        private readonly DiagnosticDescriptor itemNotNullOnMethod;

        public NullabilityConversionAnalyzer()
        {
            canBeNullOnMethodThatReturnsNullableValueType = CreateRemoveRuleFor("method", "CanBeNull");
            canBeNullOnMethodThatReturnsReferenceType = CreateConvertReferenceTypeRuleFor("method", "CanBeNull");
            notNullOnMethod = CreateRemoveRuleFor("method", "NotNull");

            itemCanBeNullOnMethodThatReturnsNullableValueType = CreateRemoveRuleFor("method", "Item CanBeNull");
            itemCanBeNullOnMethodThatReturnsReferenceType = CreateConvertReferenceTypeRuleFor("method", "ItemCanBeNull");
            itemNotNullOnMethod = CreateRemoveRuleFor("method", "ItemNotNull");
        }

        [ItemNotNull]
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            =>
                ImmutableArray.Create(canBeNullOnMethodThatReturnsNullableValueType,
                    canBeNullOnMethodThatReturnsReferenceType);

        public override void Initialize([NotNull] AnalysisContext context)
        {
            Guard.NotNull(context, nameof(context));

            // Possible scenarios (on field, property, method and parameter) for Symbol or ItemSymbol:
            // - [CanBeNull] on nullable value type =>  actions: remove attribute
            // - [CanBeNull] on reference type =>       actions: replace attribute with ? on (Item)Symbol
            // - [NotNull] on nullable value type =>    actions: remove attribute
            // - [NotNull] on reference type =>         actions: remove attribute

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

            context.RegisterSymbolAction(AnalyzeField, SymbolKind.Field);
            context.RegisterSymbolAction(AnalyzeProperty, SymbolKind.Property);
            context.RegisterSymbolAction(AnalyzeMethod, SymbolKind.Method);
            context.RegisterSyntaxNodeAction(c => AnalyzeParameter(SyntaxToSymbolContext(c)), SyntaxKind.Parameter);
        }

        private void AnalyzeField(SymbolAnalysisContext context)
        {
            // TODO
        }

        private void AnalyzeProperty(SymbolAnalysisContext context)
        {
            // TODO
        }

        private void AnalyzeMethod(SymbolAnalysisContext context)
        {
            var method = (IMethodSymbol) context.Symbol;

            ReportForAttribute(context, method, method.ReturnType, "CanBeNullAttribute", false, true,
                canBeNullOnMethodThatReturnsNullableValueType, canBeNullOnMethodThatReturnsReferenceType);
            ReportForAttribute(context, method, method.ReturnType, "NotNullAttribute", false, false, notNullOnMethod,
                null);

            ReportForAttribute(context, method, method.ReturnType, "ItemCanBeNullAttribute", true, true,
                itemCanBeNullOnMethodThatReturnsNullableValueType, itemCanBeNullOnMethodThatReturnsReferenceType);
            ReportForAttribute(context, method, method.ReturnType, "ItemNotNullAttribute", true, false,
                itemNotNullOnMethod, null);
        }

        private void ReportForAttribute(SymbolAnalysisContext context, [NotNull] ISymbol symbol,
            [NotNull] ITypeSymbol symbolType, [NotNull] string attributeName, bool appliesToItem, bool forCanBeNull,
            [NotNull] DiagnosticDescriptor removeRule, [CanBeNull] DiagnosticDescriptor replaceRule)
        {
            AttributeData attribute =
                symbol.GetAttributes().FirstOrDefault(attr => attr.AttributeClass.Name == attributeName);
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

        private void AnalyzeParameter(SymbolAnalysisContext context)
        {
            // TODO
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
    }

    public sealed class ConversionFixTarget
    {
        public ConversionFixAction FixAction { get; }

        [NotNull]
        public string AttributeName { get; }

        public bool AppliesToItem { get; }

        public ConversionFixTarget(ConversionFixAction fixAction, [NotNull] string attributeName, bool appliesToItem)
        {
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