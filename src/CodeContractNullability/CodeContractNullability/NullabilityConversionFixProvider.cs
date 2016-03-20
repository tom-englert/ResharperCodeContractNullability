using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;

namespace CodeContractNullability
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(CodeContractNullabilityCodeFixProvider))]
    [Shared]
    public class NullabilityConversionFixProvider : CodeFixProvider
    {
        [ItemNotNull]
        public override sealed ImmutableArray<string> FixableDiagnosticIds
            => ImmutableArray.Create(NullabilityConversionAnalyzer.DiagnosticId);

        [NotNull]
        public override sealed FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        [NotNull]
        public override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            foreach (Diagnostic diagnostic in context.Diagnostics)
            {
                SyntaxNode syntaxRoot =
                    await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
                SyntaxNode targetSyntax = syntaxRoot.FindNode(context.Span);

                ConversionFixTarget fixTarget = ConversionFixTarget.Parse(diagnostic.Properties);

                var methodSyntax = targetSyntax as MethodDeclarationSyntax;
                if (methodSyntax != null)
                {
                    RegisterFixesForSyntaxNode(context, methodSyntax, methodSyntax.ReturnType, diagnostic, fixTarget);
                    continue;
                }

                var indexerSyntax = targetSyntax as IndexerDeclarationSyntax;
                if (indexerSyntax != null)
                {
                    RegisterFixesForSyntaxNode(context, indexerSyntax, indexerSyntax.Type, diagnostic, fixTarget);
                    continue;
                }

                var propertySyntax = targetSyntax as PropertyDeclarationSyntax;
                if (propertySyntax != null)
                {
                    RegisterFixesForSyntaxNode(context, propertySyntax, propertySyntax.Type, diagnostic, fixTarget);
                    continue;
                }

                var parameterSyntax = targetSyntax as ParameterSyntax;
                if (parameterSyntax != null)
                {
                    RegisterFixesForSyntaxNode(context, parameterSyntax, parameterSyntax.Type, diagnostic, fixTarget);
                    continue;
                }

                FieldDeclarationSyntax fieldSyntax = targetSyntax is VariableDeclaratorSyntax
                    ? targetSyntax.GetAncestorOrThis<FieldDeclarationSyntax>()
                    : null;
                if (fieldSyntax != null)
                {
                    RegisterFixesForSyntaxNode(context, fieldSyntax, fieldSyntax.Declaration.Type, diagnostic, fixTarget);
                }
            }
        }

        private void RegisterFixesForSyntaxNode(CodeFixContext context, [NotNull] SyntaxNode targetSyntax,
            [NotNull] TypeSyntax typeSyntax, [NotNull] Diagnostic diagnostic, [NotNull] ConversionFixTarget fixTarget)
        {
            bool includeQuestionMark = fixTarget.FixAction == ConversionFixAction.ReplaceAttributeWithQuestionMark;
            string title = includeQuestionMark
                ? $"Replace {fixTarget.AttributeName} with nullable reference"
                : $"Remove {fixTarget.AttributeName}";

            CodeAction codeAction = CodeAction.Create(title,
                token => ChangeDocumentAsync(targetSyntax, typeSyntax, context, fixTarget, includeQuestionMark), title);
            context.RegisterCodeFix(codeAction, diagnostic);
        }

        [ItemNotNull]
        private static async Task<Document> ChangeDocumentAsync([NotNull] SyntaxNode targetSyntax,
            [NotNull] TypeSyntax typeSyntax, CodeFixContext context, [NotNull] ConversionFixTarget fixTarget,
            bool includeQuestionMark)
        {
            if (targetSyntax is FieldDeclarationSyntax)
            {
                targetSyntax = ((FieldDeclarationSyntax) targetSyntax).Declaration.Variables.First();
            }

            SemanticModel model =
                await context.Document.GetSemanticModelAsync(context.CancellationToken).ConfigureAwait(false);
            ISymbol targetSymbol = model.GetDeclaredSymbol(targetSyntax);

            AttributeData attributeData =
                targetSymbol.GetAttributes().First(attr => attr.AttributeClass.Name == fixTarget.AttributeName);
            SyntaxNode attributeSyntax =
                await
                    attributeData.ApplicationSyntaxReference.GetSyntaxAsync(context.CancellationToken)
                        .ConfigureAwait(false);

            DocumentEditor editor =
                await DocumentEditor.CreateAsync(context.Document, CancellationToken.None).ConfigureAwait(false);

            // TODO: Preserve leading whitespace and comments!
            editor.RemoveNode(attributeSyntax);

            if (includeQuestionMark)
            {
                string newTypeName = AddQuestionMark(typeSyntax.ToString(), fixTarget.AppliesToItem);

                TypeSyntax nullableTypeSyntax =
                    SyntaxFactory.ParseTypeName(newTypeName)
                        .WithAdditionalAnnotations(Simplifier.Annotation, Formatter.Annotation);

                editor.ReplaceNode(typeSyntax, nullableTypeSyntax);
            }

            return editor.GetChangedDocument();
        }

        [NotNull]
        private static string AddQuestionMark([NotNull] string type, bool appliesToItem)
        {
            if (appliesToItem)
            {
                int closingAngleIndex = type.LastIndexOf('>');
                if (closingAngleIndex != -1)
                {
                    string leftPart = type.Substring(0, closingAngleIndex);
                    string rightPart = type.Substring(closingAngleIndex);

                    return leftPart + "?" + rightPart;
                }

                return type;
            }

            return type + "?";
        }
    }
}