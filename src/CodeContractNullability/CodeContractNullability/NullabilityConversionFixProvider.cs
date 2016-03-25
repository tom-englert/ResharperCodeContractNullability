using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using CodeContractNullability.Utilities;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;

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
            // TODO: Submit Roslyn bug - RemoveNode with SyntaxRemoveOptions.KeepExteriorTrivia throws away comments in:
            // public Log4NetSystemLogger([CanBeNull] /* LOST */ Type type) { }

            foreach (Diagnostic diagnostic in context.Diagnostics)
            {
                CancellationToken cancellationToken = context.CancellationToken;
                ConversionFixTarget fixTarget = ConversionFixTarget.Parse(diagnostic.Properties);

                SyntaxNode syntaxRoot =
                    await context.Document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                SyntaxNode targetSyntax = syntaxRoot.FindNode(context.Span);

                targetSyntax = TranslateField(targetSyntax);

                EditContext editContext =
                    await EditContext.CreateAsync(context.Document, cancellationToken).ConfigureAwait(false);

                var syntaxAnnotation = new SyntaxAnnotation("targetSyntax");
                targetSyntax =
                    await editContext.AnnotateSyntaxAsync(targetSyntax, syntaxAnnotation).ConfigureAwait(false);

                var typeAnnotation = new SyntaxAnnotation("typeSyntax");

                var methodSyntax = targetSyntax as MethodDeclarationSyntax;
                if (methodSyntax != null)
                {
                    await editContext.AnnotateSyntaxAsync(methodSyntax.ReturnType, typeAnnotation).ConfigureAwait(false);

                    RegisterFixesForSyntaxNode(syntaxAnnotation, typeAnnotation, diagnostic, context, editContext,
                        fixTarget);
                    continue;
                }

                var indexerSyntax = targetSyntax as IndexerDeclarationSyntax;
                if (indexerSyntax != null)
                {
                    await editContext.AnnotateSyntaxAsync(indexerSyntax.Type, typeAnnotation).ConfigureAwait(false);

                    RegisterFixesForSyntaxNode(syntaxAnnotation, typeAnnotation, diagnostic, context, editContext,
                        fixTarget);
                    continue;
                }

                var propertySyntax = targetSyntax as PropertyDeclarationSyntax;
                if (propertySyntax != null)
                {
                    await editContext.AnnotateSyntaxAsync(propertySyntax.Type, typeAnnotation).ConfigureAwait(false);

                    RegisterFixesForSyntaxNode(syntaxAnnotation, typeAnnotation, diagnostic, context, editContext,
                        fixTarget);
                    continue;
                }

                var parameterSyntax = targetSyntax as ParameterSyntax;
                if (parameterSyntax != null)
                {
                    await editContext.AnnotateSyntaxAsync(parameterSyntax.Type, typeAnnotation).ConfigureAwait(false);

                    RegisterFixesForSyntaxNode(syntaxAnnotation, typeAnnotation, diagnostic, context, editContext,
                        fixTarget);
                    continue;
                }

                var fieldSyntax = targetSyntax as FieldDeclarationSyntax;
                if (fieldSyntax != null)
                {
                    await
                        editContext.AnnotateSyntaxAsync(fieldSyntax.Declaration.Type, typeAnnotation)
                            .ConfigureAwait(false);

                    RegisterFixesForSyntaxNode(syntaxAnnotation, typeAnnotation, diagnostic, context, editContext,
                        fixTarget);
                }
            }
        }

        [NotNull]
        private static SyntaxNode TranslateField([NotNull] SyntaxNode syntax)
        {
            var fieldVariableSyntax = syntax as VariableDeclaratorSyntax;
            return fieldVariableSyntax != null
                ? fieldVariableSyntax.GetAncestorOrThis<FieldDeclarationSyntax>()
                : syntax;
        }

        private void RegisterFixesForSyntaxNode([NotNull] SyntaxAnnotation syntaxAnnotation,
            [NotNull] SyntaxAnnotation typeAnnotation, [NotNull] Diagnostic diagnostic, CodeFixContext fixContext,
            [NotNull] EditContext editContext, [NotNull] ConversionFixTarget fixTarget)
        {
            bool includeQuestionMark = fixTarget.FixAction == ConversionFixAction.ReplaceAttributeWithQuestionMark;
            string title = includeQuestionMark
                ? $"Replace {fixTarget.AttributeName} with nullable reference"
                : $"Remove {fixTarget.AttributeName}";

            CodeAction codeAction = CodeAction.Create(title,
                token =>
                    ChangeDocumentAsync(syntaxAnnotation, typeAnnotation, editContext, fixTarget, includeQuestionMark),
                fixTarget.EquivalenceKey);

            fixContext.RegisterCodeFix(codeAction, diagnostic);
        }

        [ItemCanBeNull]
        private static async Task<Document> ChangeDocumentAsync([NotNull] SyntaxAnnotation syntaxAnnotation,
            [NotNull] SyntaxAnnotation typeAnnotation, [NotNull] EditContext editContext,
            [NotNull] ConversionFixTarget fixTarget, bool includeQuestionMark)
        {
            SyntaxNode targetSyntax = await editContext.GetAnnotatedSyntaxAsync(syntaxAnnotation).ConfigureAwait(false);

            ISymbol targetSymbol = await SyntaxToSymbol(editContext, targetSyntax).ConfigureAwait(false);

            AttributeSyntax attributeSyntax =
                await
                    GetAttributeSyntaxOrNullAsync(targetSymbol, fixTarget.AttributeName, editContext.CancellationToken)
                        .ConfigureAwait(false);

            if (attributeSyntax == null)
            {
                // Attribute no longer found; do nothing.
                return editContext.Document;
            }

            // Remove attribute (preserves trivia, but removes empty blank line)
            await RemoveAttributeFromListAsync(attributeSyntax, editContext).ConfigureAwait(false);

            if (includeQuestionMark)
            {
                var typeSyntax =
                    (TypeSyntax) await editContext.GetAnnotatedSyntaxAsync(typeAnnotation).ConfigureAwait(false);

                string nullableTypeName = AddQuestionMarkToTypeName(typeSyntax.ToString(), fixTarget.AppliesToItem);

                TypeSyntax nullableTypeSyntax =
                    SyntaxFactory.ParseTypeName(nullableTypeName).WithAdditionalAnnotations(Formatter.Annotation);

                await editContext.ReplaceSyntaxAsync(typeSyntax, nullableTypeSyntax).ConfigureAwait(false);
            }

            return editContext.Document;
        }

        [ItemNotNull]
        private static async Task<ISymbol> SyntaxToSymbol([NotNull] EditContext editContext,
            [NotNull] SyntaxNode targetSyntax)
        {
            var fieldSyntax = targetSyntax as FieldDeclarationSyntax;
            if (fieldSyntax != null)
            {
                targetSyntax = fieldSyntax.Declaration.Variables.First();
            }

            SemanticModel model =
                await editContext.Document.GetSemanticModelAsync(editContext.CancellationToken).ConfigureAwait(false);
            return model.GetDeclaredSymbol(targetSyntax);
        }

        [ItemCanBeNull]
        private static async Task<AttributeSyntax> GetAttributeSyntaxOrNullAsync([NotNull] ISymbol targetSymbol,
            [NotNull] string attributeName, CancellationToken cancellationToken)
        {
            AttributeData attributeData =
                targetSymbol.GetAttributes().FirstOrDefault(attr => attr.AttributeClass.Name == attributeName);

            if (attributeData == null)
            {
                return null;
            }

            SyntaxReference syntaxReference = attributeData.ApplicationSyntaxReference;
            SyntaxNode attributeSyntax = await syntaxReference.GetSyntaxAsync(cancellationToken).ConfigureAwait(false);
            return (AttributeSyntax) attributeSyntax;
        }

        [NotNull]
        private static string AddQuestionMarkToTypeName([NotNull] string typeName, bool appliesToItem)
        {
            if (appliesToItem)
            {
                int closingAngleIndex = typeName.LastIndexOf('>');
                if (closingAngleIndex != -1)
                {
                    string leftPart = typeName.Substring(0, closingAngleIndex);
                    string rightPart = typeName.Substring(closingAngleIndex);

                    return leftPart + "?" + rightPart;
                }

                // Should never get here.
                return typeName;
            }

            return typeName + "?";
        }

        [ItemNotNull]
        private static async Task RemoveAttributeFromListAsync([NotNull] AttributeSyntax attributeSyntax,
            [NotNull] EditContext context)
        {
            var attributeListSyntax = (AttributeListSyntax) attributeSyntax.Parent;

            SyntaxNode syntaxToRemove = attributeListSyntax.Attributes.Count == 1
                ? (SyntaxNode) attributeListSyntax
                : attributeSyntax;

            await RemoveSyntaxAsync(syntaxToRemove, context).ConfigureAwait(false);
        }

        private static async Task RemoveSyntaxAsync([NotNull] SyntaxNode syntaxNode, [NotNull] EditContext context)
        {
            SyntaxNode noSpaceSyntaxNode = RemoveTrailingEndOfLine(syntaxNode);
            if (syntaxNode != noSpaceSyntaxNode)
            {
                noSpaceSyntaxNode =
                    await context.ReplaceSyntaxAsync(syntaxNode, noSpaceSyntaxNode).ConfigureAwait(false);
            }

            SyntaxNode parent = noSpaceSyntaxNode.Parent;
            SyntaxNode parentRemoved =
                parent.RemoveNode(noSpaceSyntaxNode, SyntaxRemoveOptions.KeepExteriorTrivia)
                    .WithAdditionalAnnotations(Formatter.Annotation);

            await context.ReplaceSyntaxAsync(parent, parentRemoved).ConfigureAwait(false);
        }

        [NotNull]
        private static T RemoveTrailingEndOfLine<T>([NotNull] T syntax) where T : SyntaxNode
        {
            List<SyntaxTrivia> trailingTrivia = syntax.GetTrailingTrivia().ToList();

            for (int index = 0; index < trailingTrivia.Count && !IsEndOfLine(trailingTrivia[index]); index++)
            {
                if (trailingTrivia[index].Kind() == SyntaxKind.WhitespaceTrivia)
                {
                    trailingTrivia.RemoveAt(index);
                    index--;
                }
                else
                {
                    // Non-whitespace found, so this line cannot be removed.
                    return syntax;
                }
            }

            if (trailingTrivia.Count > 0 && IsEndOfLine(trailingTrivia[0]))
            {
                // Remove line break.
                trailingTrivia.RemoveAt(0);

                if (LeadingTriviaOnSameLineIsEmptyOrWhitespace(syntax))
                {
                    return syntax.WithTrailingTrivia(trailingTrivia);
                }
            }

            return syntax;
        }

        private static bool LeadingTriviaOnSameLineIsEmptyOrWhitespace([NotNull] SyntaxNode syntax)
        {
            SyntaxTriviaList leadingTrivia = syntax.GetLeadingTrivia();

            for (int index = leadingTrivia.Count - 1; index >= 0; index--)
            {
                if (IsEndOfLine(leadingTrivia[index]))
                {
                    return true;
                }
                if (leadingTrivia[index].Kind() != SyntaxKind.WhitespaceTrivia)
                {
                    return false;
                }
            }

            return true;
        }

        private static bool IsEndOfLine(SyntaxTrivia trivia)
        {
            return trivia.Kind() == SyntaxKind.EndOfLineTrivia;
        }

        private sealed class EditContext
        {
            [NotNull]
            public Document Document { get; private set; }

            [NotNull]
            private DocumentEditor editor;

            public CancellationToken CancellationToken { get; }

            private EditContext([NotNull] Document document, [NotNull] DocumentEditor editor,
                CancellationToken cancellationToken)
            {
                Document = document;
                this.editor = editor;
                CancellationToken = cancellationToken;
            }

            [ItemNotNull]
            public static async Task<EditContext> CreateAsync([NotNull] Document document,
                CancellationToken cancellationToken)
            {
                Guard.NotNull(document, nameof(document));

                DocumentEditor editor =
                    await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

                return new EditContext(document, editor, cancellationToken);
            }

            [ItemNotNull]
            public async Task<T> AnnotateSyntaxAsync<T>([NotNull] T syntax, [NotNull] SyntaxAnnotation annotation)
                where T : SyntaxNode
            {
                Guard.NotNull(syntax, nameof(syntax));
                Guard.NotNull(annotation, nameof(annotation));

                return (T) await ReplaceSyntaxWithAnnotationAsync(syntax, syntax, annotation).ConfigureAwait(false);
            }

            [ItemNotNull]
            public async Task<SyntaxNode> GetAnnotatedSyntaxAsync([NotNull] SyntaxAnnotation annotation)
            {
                Guard.NotNull(annotation, nameof(annotation));

                SyntaxNode root = await Document.GetSyntaxRootAsync(CancellationToken).ConfigureAwait(false);
                return root.GetAnnotatedNodes(annotation).First();
            }

            [ItemNotNull]
            public async Task<SyntaxNode> ReplaceSyntaxAsync([NotNull] SyntaxNode syntax,
                [NotNull] SyntaxNode replacementSyntax)
            {
                Guard.NotNull(syntax, nameof(syntax));
                Guard.NotNull(replacementSyntax, nameof(replacementSyntax));

                var trackAnnotation = new SyntaxAnnotation();
                return
                    await
                        ReplaceSyntaxWithAnnotationAsync(syntax, replacementSyntax, trackAnnotation)
                            .ConfigureAwait(false);
            }

            [ItemNotNull]
            private async Task<SyntaxNode> ReplaceSyntaxWithAnnotationAsync([NotNull] SyntaxNode syntax,
                [NotNull] SyntaxNode replacementSyntax, [NotNull] SyntaxAnnotation trackAnnotation)
            {
                replacementSyntax = replacementSyntax.WithAdditionalAnnotations(trackAnnotation);

                editor.ReplaceNode(syntax, replacementSyntax);
                Document = editor.GetChangedDocument();

                SyntaxNode newSyntax = await GetAnnotatedSyntaxAsync(trackAnnotation).ConfigureAwait(false);

                editor = await DocumentEditor.CreateAsync(Document, CancellationToken).ConfigureAwait(false);

                return newSyntax;
            }
        }
    }
}