using CodeContractNullability.Utilities;
using JetBrains.Annotations;

namespace CodeContractNullability
{
    public static class StringExtensions
    {
        [NotNull]
        public static string ToCamelCase([NotNull] this string text)
        {
            Guard.NotNull(text, nameof(text));

            return text.Length == 1
                ? text.ToLowerInvariant()
                : text.Substring(0, 1).ToLowerInvariant() + text.Substring(1);
        }
    }
}