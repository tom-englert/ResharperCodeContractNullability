﻿using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using CodeContractNullability.Utilities;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CodeContractNullability
{
    /// <summary>
    /// Within a compilation, caches per-file whether it consists of generated code.
    /// </summary>
    /// <remarks>
    /// Inspired by StyleCop source code. Path:
    /// DotNetAnalyzers/StyleCopAnalyzers/StyleCop.Analyzers/StyleCop.Analyzers/GeneratedCodeAnalysisExtensions.cs
    /// <see
    ///     href="https://github.com/DotNetAnalyzers/StyleCopAnalyzers/blob/eea978e9953973f32bce5d8e376a14a5ee0e6d9a/StyleCop.Analyzers/StyleCop.Analyzers/GeneratedCodeAnalysisExtensions.cs" />
    /// </remarks>
    public sealed class GeneratedCodeDocumentCache
    {
        [NotNull]
        private readonly FileCommentScanner scanner = new FileCommentScanner();

        [NotNull]
        private readonly ConcurrentDictionary<string, bool> fileResultCache =
            new ConcurrentDictionary<string, bool>(StringComparer.OrdinalIgnoreCase);

        public bool IsInGeneratedCodeDocument([NotNull] ISymbol symbol, CancellationToken cancellationToken)
        {
            Guard.NotNull(symbol, nameof(symbol));

            // Return false when not all locations have generated comment; for example, partial classes.

            IEnumerable<Location> locations = symbol.Locations.Where(location => location.IsInSource);
            var trees = new HashSet<SyntaxTree>(locations.Select(location => location.SourceTree));

            return trees.Any() && trees.All(tree => IsGeneratedCodeDocument(tree, cancellationToken));
        }

        private bool IsGeneratedCodeDocument([NotNull] SyntaxTree syntaxTree, CancellationToken cancellationToken)
        {
            Guard.NotNull(syntaxTree, nameof(syntaxTree));

            if (scanner.IsFileNameGenerated(syntaxTree.FilePath))
            {
                return true;
            }

            string key = syntaxTree.FilePath;
            if (!fileResultCache.ContainsKey(key))
            {
                fileResultCache[key] = scanner.HasAutoGeneratedCommentHeader(syntaxTree, cancellationToken);
            }

            return fileResultCache[key];
        }

        /// <summary>
        /// Determines whether a source code file is auto-generated, by looking at its top-level comments.
        /// </summary>
        private sealed class FileCommentScanner
        {
            [NotNull]
            private static readonly Regex FileNameRegex =
                new Regex(
                    @"(^TemporaryGeneratedFile_.*|^assemblyinfo|^assemblyattributes|\.(g\.i|g|designer|generated|assemblyattributes))\.(cs|vb)$",
                    RegexOptions.IgnoreCase | RegexOptions.ExplicitCapture);

            public bool IsFileNameGenerated([CanBeNull] string filePath)
            {
                if (filePath != null)
                {
                    string filename = Path.GetFileName(filePath);
                    return FileNameRegex.IsMatch(filename);
                }
                return false;
            }

            public bool HasAutoGeneratedCommentHeader([NotNull] SyntaxTree tree, CancellationToken cancellationToken)
            {
                Guard.NotNull(tree, nameof(tree));

                SyntaxNode root = tree.GetRoot(cancellationToken);
                if (root != null)
                {
                    SyntaxTriviaList? trivia = GetTopOfFileTriviaOrNull(root);
                    if (trivia != null)
                    {
                        return trivia.Value.Any(t => IsCodeComment(t) && t.ToString().Contains("<auto-generated"));
                    }
                }

                return false;
            }

            [CanBeNull]
            private SyntaxTriviaList? GetTopOfFileTriviaOrNull([NotNull] SyntaxNode root)
            {
                SyntaxToken firstToken = root.GetFirstToken();

                if (firstToken == default(SyntaxToken))
                {
                    SyntaxToken lastToken = ((CompilationUnitSyntax)root).EndOfFileToken;
                    return !lastToken.HasLeadingTrivia ? (SyntaxTriviaList?)null : lastToken.LeadingTrivia;
                }

                return !firstToken.HasLeadingTrivia ? (SyntaxTriviaList?)null : firstToken.LeadingTrivia;
            }

            private bool IsCodeComment(SyntaxTrivia trivia)
            {
                return trivia.IsKind(SyntaxKind.SingleLineCommentTrivia) || trivia.IsKind(SyntaxKind.MultiLineCommentTrivia);
            }
        }
    }
}
