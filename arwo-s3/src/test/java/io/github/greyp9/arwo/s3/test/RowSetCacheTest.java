package io.github.greyp9.arwo.s3.test;

import io.github.greyp9.arwo.core.cache.CacheRowSetSource;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.s3.connection.S3Connection;
import io.github.greyp9.arwo.s3.connection.S3ConnectionFactory;
import io.github.greyp9.arwo.s3.connection.S3ConnectionResource;
import io.github.greyp9.arwo.s3.data.S3RowSetSource;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

public class RowSetCacheTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testRowSetCache() throws Exception {
        final String regionName = System.getProperty("region");
        final String bucketName = System.getProperty("bucket");
        logger.info(String.format("REGION=%s, BUCKET=%s", regionName, bucketName));
        Assumptions.assumeTrue(Value.isData(regionName, bucketName));

        final S3ConnectionFactory factory = new S3ConnectionFactory(regionName, bucketName);
        final String name = String.format("%s/%s", regionName, bucketName);
        final Collection<String> prefixes = new ArrayList<>();
        prefixes.add("");
        final ResourceCache cache = new ResourceCache(null);
        Assertions.assertEquals(0L, cache.getSize());

        try (S3ConnectionResource connectionResource = (S3ConnectionResource) factory.create(name)) {
            final S3Connection connection = connectionResource.getConnection();
            Assertions.assertEquals(0L, connection.getCount());
            while (!prefixes.isEmpty()) {
                final String prefix = prefixes.iterator().next();
                prefixes.remove(prefix);
                final RowSetSource rowSetSource = new CacheRowSetSource(
                        cache, new S3RowSetSource(connection, "baseURI", bucketName, prefix), prefix);
                final RowSet rowSet = rowSetSource.getRowSet();
                Assertions.assertTrue(rowSet.getRows() > 0);
                if (prefix.isEmpty()) {  // only recurse one level
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
            Assertions.assertTrue(connection.getCount() > 0);
            // Assertions.assertEquals(4, connection.getCount());  // dependent on target bucket
        }
    }
}
