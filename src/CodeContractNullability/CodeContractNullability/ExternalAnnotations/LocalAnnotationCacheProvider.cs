using System;
using System.Threading;
using CodeContractNullability.ExternalAnnotations.Storage;
using CodeContractNullability.ExternalAnnotations.Storage.FileSystem;
using CodeContractNullability.Utilities;
using JetBrains.Annotations;

namespace CodeContractNullability.ExternalAnnotations
{
    public sealed class LocalAnnotationCacheProvider : ICacheProvider<ExternalAnnotationsMap>
    {
        [NotNull]
        [ItemNotNull]
        private readonly Lazy<ExternalAnnotationsMap> localCache;

        public LocalAnnotationCacheProvider([NotNull] IFileSystem fileSystem)
        {
            Guard.NotNull(fileSystem, nameof(fileSystem));

            localCache = new Lazy<ExternalAnnotationsMap>(new FolderExternalAnnotationsLoader(fileSystem).Create,
                LazyThreadSafetyMode.ExecutionAndPublication);
        }

        public ExternalAnnotationsMap GetValue()
        {
            return localCache.Value;
        }
    }
}
