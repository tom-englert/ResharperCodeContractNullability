using System;
using System.Collections.Generic;
using System.IO;
using JetBrains.Annotations;

namespace CodeContractNullability.ExternalAnnotations.Storage.FileSystem
{
    public sealed class WindowsFileSystem : IFileSystem
    {
        public IDirectory Directory { get; } = new SystemDirectory();

        public IFile File { get; } = new SystemFile();

        public IFileInfo CreateFileInfo(string path)
        {
            return new SystemFileInfo(path);
        }

        public IFileSystemWatcher CreateWatcher(string path, string filter)
        {
            return new SystemFileSystemWatcher(path, filter);
        }

        private sealed class SystemDirectory : IDirectory
        {
            public bool Exists(string path)
            {
                return System.IO.Directory.Exists(path);
            }

            public string[] GetDirectories(string path, string searchPattern)
            {
                return System.IO.Directory.GetDirectories(path, searchPattern);
            }

            public void CreateDirectory(string path)
            {
                System.IO.Directory.CreateDirectory(path);
            }

            public IEnumerable<string> EnumerateFiles(string path, string searchPattern, SearchOption searchOption)
            {
                return System.IO.Directory.EnumerateFiles(path, searchPattern, searchOption);
            }
        }

        private sealed class SystemFile : IFile
        {
            public bool Exists(string path)
            {
                return System.IO.File.Exists(path);
            }

            public Stream OpenRead(string path)
            {
                return System.IO.File.OpenRead(path);
            }

            public Stream Create(string path)
            {
                return System.IO.File.Create(path);
            }

            public StreamReader OpenText(string path)
            {
                return System.IO.File.OpenText(path);
            }
        }

        private sealed class SystemFileInfo : IFileInfo
        {
            [NotNull]
            private readonly FileInfo fileInfo;

            public SystemFileInfo([NotNull] string path)
            {
                fileInfo = new FileInfo(path);
            }

            public DateTime LastWriteTimeUtc => fileInfo.LastWriteTimeUtc;
        }

        private sealed class SystemFileSystemWatcher : IFileSystemWatcher
        {
            [NotNull]
            private readonly FileSystemWatcher watcher;

            public event FileSystemEventHandler Changed;
            public event FileSystemEventHandler Created;
            public event FileSystemEventHandler Deleted;
            public event RenamedEventHandler Renamed;

            public bool EnableRaisingEvents
            {
                get
                {
                    return watcher.EnableRaisingEvents;
                }
                set
                {
                    watcher.EnableRaisingEvents = value;
                }
            }

            public SystemFileSystemWatcher([NotNull] string path, [NotNull] string filter)
            {
                watcher = new FileSystemWatcher(path, filter);

                watcher.Changed += Changed;
                watcher.Created += Created;
                watcher.Deleted += Deleted;
                watcher.Renamed += Renamed;
            }

            public void Dispose()
            {
                watcher.Changed -= Changed;
                watcher.Created -= Created;
                watcher.Deleted -= Deleted;
                watcher.Renamed -= Renamed;

                watcher.Dispose();
            }
        }
    }
}
