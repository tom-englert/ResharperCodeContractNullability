using System;

namespace CodeContractNullability.ExternalAnnotations.Storage.FileSystem
{
    public interface IFileInfo
    {
        DateTime LastWriteTimeUtc { get; }
    }
}