using System.Collections.Generic;
using System.IO;
using JetBrains.Annotations;

namespace CodeContractNullability.ExternalAnnotations.Storage.FileSystem
{
    public interface IDirectory
    {
        bool Exists([CanBeNull] string path);

        [NotNull]
        [ItemNotNull]
        string[] GetDirectories([NotNull] string path, [NotNull] string searchPattern);

        void CreateDirectory([NotNull] string path);

        [NotNull]
        [ItemNotNull]
        IEnumerable<string> EnumerateFiles([NotNull] string path, [NotNull] string searchPattern, SearchOption searchOption);
    }
}