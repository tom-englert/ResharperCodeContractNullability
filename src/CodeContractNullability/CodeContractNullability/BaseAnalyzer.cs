﻿using System.Collections.Immutable;
using CodeContractNullability.ExternalAnnotations;
using CodeContractNullability.NullabilityAttributes;
using CodeContractNullability.SymbolAnalysis;
using CodeContractNullability.Utilities;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;

namespace CodeContractNullability
{
    /// <summary>
    /// Common functionality for all the diagnostics analyzers that are included in this project.
    /// </summary>
    public abstract class BaseAnalyzer : DiagnosticAnalyzer
    {
        protected const string Category = "Nullability";
        private readonly bool appliesToItem;

        [NotNull]
        private readonly DiagnosticDescriptor ruleForField;

        [NotNull]
        private readonly DiagnosticDescriptor ruleForProperty;

        [NotNull]
        private readonly DiagnosticDescriptor ruleForMethodReturnValue;

        [NotNull]
        private readonly DiagnosticDescriptor ruleForParameter;

        [NotNull]
        protected abstract DiagnosticDescriptor CreateRuleFor([NotNull] string memberTypePascalCase);

        [ItemNotNull]
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            => ImmutableArray.Create(ruleForField, ruleForProperty, ruleForMethodReturnValue, ruleForParameter);

        [NotNull]
        public ExtensionPoint<INullabilityAttributeProvider> NullabilityAttributeProvider { get; } =
            new ExtensionPoint<INullabilityAttributeProvider>(() => new CachingNullabilityAttributeProvider());

        [NotNull]
        public ExtensionPoint<IExternalAnnotationsResolver> ExternalAnnotationsResolver { get; } =
            new ExtensionPoint<IExternalAnnotationsResolver>(() => new CachingExternalAnnotationsResolver());

        protected BaseAnalyzer(bool appliesToItem)
        {
            this.appliesToItem = appliesToItem;

            // ReSharper disable DoNotCallOverridableMethodsInConstructor
            ruleForField = CreateRuleFor("Field");
            ruleForProperty = CreateRuleFor("Property");
            ruleForMethodReturnValue = CreateRuleFor("Method");
            ruleForParameter = CreateRuleFor("Parameter");
            // ReSharper restore DoNotCallOverridableMethodsInConstructor
        }

        public override void Initialize([NotNull] AnalysisContext context)
        {
            Guard.NotNull(context, nameof(context));

            context.RegisterCompilationStartAction(StartAnalyzeCompilation);
        }

        private void StartAnalyzeCompilation([NotNull] CompilationStartAnalysisContext context)
        {
            Guard.NotNull(context, nameof(context));

            var settingsReader = new SettingsReader(context.Options);
            AnalyzerSettings settings = settingsReader.GetSettings(context.CancellationToken);

            NullabilityAttributeSymbols nullSymbols =
                NullabilityAttributeProvider.GetCached().GetSymbols(context.Compilation, context.CancellationToken);
            if (nullSymbols == null)
            {
                // Nullability attributes not found; keep silent.
                return;
            }

            IExternalAnnotationsResolver resolver = ExternalAnnotationsResolver.GetCached();
            resolver.EnsureScanned();

            var generatedCodeCache = new GeneratedCodeDocumentCache();
            var typeCache = new FrameworkTypeCache(context.Compilation);
            var factory = new SymbolAnalyzerFactory(resolver, generatedCodeCache, typeCache, settings, appliesToItem);

            ImmutableDictionary<string, string> properties = nullSymbols.GetMetadataNamesAsProperties();

            context.RegisterSymbolAction(c => AnalyzeField(c, factory, properties), SymbolKind.Field);
            context.RegisterSymbolAction(c => AnalyzeProperty(c, factory, properties), SymbolKind.Property);
            context.RegisterSymbolAction(c => AnalyzeMethod(c, factory, properties), SymbolKind.Method);
            context.RegisterSyntaxNodeAction(c => AnalyzeParameter(SyntaxToSymbolContext(c), factory, properties),
                SyntaxKind.Parameter);
        }

        private void AnalyzeField(SymbolAnalysisContext context, [NotNull] SymbolAnalyzerFactory factory,
            [NotNull] ImmutableDictionary<string, string> properties)
        {
            FieldAnalyzer analyzer = factory.GetFieldAnalyzer(context);
            analyzer.Analyze(ruleForField, properties);
        }

        private void AnalyzeProperty(SymbolAnalysisContext context, [NotNull] SymbolAnalyzerFactory factory,
            [NotNull] ImmutableDictionary<string, string> properties)
        {
            PropertyAnalyzer analyzer = factory.GetPropertyAnalyzer(context);
            analyzer.Analyze(ruleForProperty, properties);
        }

        private void AnalyzeMethod(SymbolAnalysisContext context, [NotNull] SymbolAnalyzerFactory factory,
            [NotNull] ImmutableDictionary<string, string> properties)
        {
            MethodReturnValueAnalyzer analyzer = factory.GetMethodReturnValueAnalyzer(context);
            analyzer.Analyze(ruleForMethodReturnValue, properties);
        }

        private void AnalyzeParameter(SymbolAnalysisContext context, [NotNull] SymbolAnalyzerFactory factory,
            [NotNull] ImmutableDictionary<string, string> properties)
        {
            ParameterAnalyzer analyzer = factory.GetParameterAnalyzer(context);
            analyzer.Analyze(ruleForParameter, properties);
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
}