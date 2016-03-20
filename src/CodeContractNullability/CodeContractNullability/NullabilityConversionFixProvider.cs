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
        public override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            foreach (Diagnostic diagnostic in context.Diagnostics)
            {
                SyntaxNode syntaxRoot =
                    await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
                SyntaxNode targetSyntax = syntaxRoot.FindNode(context.Span);

                ConversionFixTarget fixTarget = ConversionFixTarget.Parse(diagnostic.Properties);

                if (targetSyntax is MethodDeclarationSyntax)
                {
                    RegisterFixesForSyntaxNode(context, targetSyntax, diagnostic, fixTarget);
                }
            }
        }

        private void RegisterFixesForSyntaxNode(CodeFixContext context, [NotNull] SyntaxNode syntaxNode,
            [NotNull] Diagnostic diagnostic, [NotNull] ConversionFixTarget fixTarget)
        {
            if (fixTarget.FixAction == ConversionFixAction.RemoveAttribute)
            {
                context.RegisterCodeFix(
                    CodeAction.Create("Remove " + fixTarget.AttributeName,
                        token => RemoveAttributeFromDocument(context, syntaxNode, fixTarget)), diagnostic);
            }
            else if (fixTarget.FixAction == ConversionFixAction.ReplaceAttributeWithQuestionMark)
            {
                context.RegisterCodeFix(
                    CodeAction.Create("title", token => ReplaceWithQuestionMark(context, syntaxNode, fixTarget)),
                    diagnostic);
            }
        }

        [ItemNotNull]
        private async Task<Document> ReplaceWithQuestionMark(CodeFixContext context, [NotNull] SyntaxNode syntaxNode,
            [NotNull] ConversionFixTarget fixTarget)
        {
            var methodSyntax = (MethodDeclarationSyntax) syntaxNode;

            Document document = context.Document; //await RemoveAttributeFromDocument(context, methodSyntax, fixTarget);
            DocumentEditor editor =
                await DocumentEditor.CreateAsync(document, CancellationToken.None).ConfigureAwait(false);

            TypeSyntax returnSyntax = methodSyntax.ReturnType;

            // TODO: How to add ? such as Task<string?>  -- next fails on Task<List<string>>>
            TypeSyntax nullableTypeSyntax =
                SyntaxFactory.ParseTypeName(!fixTarget.AppliesToItem
                    ? returnSyntax + "?"
                    : returnSyntax.ToString().Replace(">", "?>"))
                    .WithAdditionalAnnotations(Simplifier.Annotation, Formatter.Annotation);

            editor.ReplaceNode(returnSyntax, nullableTypeSyntax);

            return editor.GetChangedDocument();
        }

        [ItemNotNull]
        private static async Task<Document> RemoveAttributeFromDocument(CodeFixContext context,
            [NotNull] SyntaxNode syntaxNode, [NotNull] ConversionFixTarget fixTarget)
        {
            SemanticModel model = await context.Document.GetSemanticModelAsync().ConfigureAwait(false);
            ISymbol symbol = model.GetDeclaredSymbol(syntaxNode);

            AttributeData attrSymbol =
                symbol.GetAttributes().First(attr => attr.AttributeClass.Name == fixTarget.AttributeName);
            SyntaxNode attrSyntax = await attrSymbol.ApplicationSyntaxReference.GetSyntaxAsync().ConfigureAwait(false);

            DocumentEditor editor =
                await DocumentEditor.CreateAsync(context.Document, CancellationToken.None).ConfigureAwait(false);
            editor.RemoveNode(attrSyntax);
            return editor.GetChangedDocument();
        }
    }
}