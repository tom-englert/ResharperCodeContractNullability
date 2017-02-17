using System.IO;
using JetBrains.Annotations;

namespace CodeContractNullability.ExternalAnnotations.Storage.FileSystem
{
    public interface IFile
    {
        bool Exists([CanBeNull] string path);

        [NotNull]
        Stream OpenRead([NotNull] string path);

        [NotNull]
        Stream Create([NotNull] string path);

        [NotNull]
        StreamReader OpenText([NotNull] string path);
    }
}