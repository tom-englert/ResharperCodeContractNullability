using JetBrains.Annotations;

namespace CodeContractNullability.ExternalAnnotations.Storage.FileSystem
{
    public interface IFileSystem
    {
        [NotNull]
        IDirectory Directory { get; }

        [NotNull]
        IFile File { get; }

        [NotNull]
        IFileInfo CreateFileInfo([NotNull] string path);

        [NotNull]
        IFileSystemWatcher CreateWatcher([NotNull] string path, [NotNull] string filter);
    }
}
