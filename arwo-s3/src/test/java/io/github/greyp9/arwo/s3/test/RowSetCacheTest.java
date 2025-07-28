package io.github.greyp9.arwo.s3.test;

import io.github.greyp9.arwo.core.cache.CacheRowSetSource;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.s3.data.S3RowSetSource;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.S3ClientBuilder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

public class RowSetCacheTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testRowSetCache() throws Exception {
        final String bucketName = System.getProperty("bucket");
        final String regionName = System.getProperty("region");
        logger.info(String.format("BUCKET=%s, REGION=%s", bucketName, regionName));
        Assumptions.assumeTrue(Value.isData(bucketName, regionName));
        final S3ClientBuilder s3ClientBuilder = S3Client.builder()
                .credentialsProvider(DefaultCredentialsProvider.builder().build())  // "~/.aws/credentials"?
                .region(Region.of(regionName));
        final Collection<String> prefixes = new ArrayList<>();
        prefixes.add("");
        final ResourceCache cache = new ResourceCache(null);
        Assertions.assertEquals(0L, cache.getSize());
        try (S3Client s3Client = s3ClientBuilder.build()) {
            while (!prefixes.isEmpty()) {
                final String prefix = prefixes.iterator().next();
                prefixes.remove(prefix);
                final RowSetSource rowSetSource = new CacheRowSetSource(
                        cache, new S3RowSetSource(s3Client, "baseURI", bucketName, prefix), prefix);
                final RowSet rowSet = rowSetSource.getRowSet();
                Assertions.assertTrue(rowSet.getRows() > 0);
                if (prefix.isEmpty()) {
                    final Iterator<Row> iterator = rowSet.iterator();
                    while (iterator.hasNext()) {
                        final Row row = iterator.next();
                        final String prefixSub = row.getString(1);
                        if (prefixSub.endsWith("/")) {
                            prefixes.add(prefix + prefixSub);
                        }
                    }
                }
            }
        }
        Assertions.assertTrue(cache.getSize() > 0);
    }
}
