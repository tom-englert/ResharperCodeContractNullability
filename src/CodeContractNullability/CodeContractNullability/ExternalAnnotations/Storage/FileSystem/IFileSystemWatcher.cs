using System;
using System.IO;

namespace CodeContractNullability.ExternalAnnotations.Storage.FileSystem
{
    public interface IFileSystemWatcher : IDisposable
    {
        event FileSystemEventHandler Changed;
        event FileSystemEventHandler Created;
        event FileSystemEventHandler Deleted;
        event RenamedEventHandler Renamed;

        bool EnableRaisingEvents { get; set; }
    }
}
