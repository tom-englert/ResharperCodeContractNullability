using CodeContractNullability.ExternalAnnotations;
using CodeContractNullability.ExternalAnnotations.Storage.FileSystem;
using Xunit;

namespace CodeContractNullability.Test.Specs
{
    public sealed class ScanningForExternalAnnotationsSpecs
    {
        [Fact(Skip = "TODO")]
        public void XXX()
        {
            IFileSystem fileSystem = null /*
                new FileSystemBuilder()
                    .Build()*/;

            var resolver = new CachingExternalAnnotationsResolver(fileSystem,
                new LocalAnnotationCacheProvider(fileSystem));

            resolver.EnsureScanned();
        }
    }
}