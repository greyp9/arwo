package io.github.greyp9.arwo.s3.test;

import io.github.greyp9.arwo.core.value.Value;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.S3ClientBuilder;
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request;
import software.amazon.awssdk.services.s3.model.ListObjectsV2Response;
import software.amazon.awssdk.services.s3.model.S3Object;

import java.util.List;
import java.util.logging.Logger;

public class S3Test {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testListObjects() {
        final String bucketName = System.getProperty("bucket");
        final String regionName = System.getProperty("region");
        logger.info(String.format("BUCKET=%s, REGION=%s", bucketName, regionName));
        Assumptions.assumeTrue(Value.isData(bucketName, regionName));
        final S3ClientBuilder s3ClientBuilder = S3Client.builder()
                .credentialsProvider(DefaultCredentialsProvider.builder().build())  // "~/.aws/credentials"?
                .region(Region.of(regionName));
        try (S3Client s3Client = s3ClientBuilder.build()) {
            final ListObjectsV2Request listObjectsRequest = ListObjectsV2Request.builder()
                    .bucket(bucketName).prefix("").delimiter("/").build();
            final ListObjectsV2Response listObjectsResponse = s3Client.listObjectsV2(listObjectsRequest);
            final List<S3Object> contents = listObjectsResponse.contents();
            logger.info(String.format("COUNT=%d", contents.size()));
            for (S3Object s3Object : contents) {
                logger.info(String.format("S3OBJECT=%s", s3Object.key()));
            }
        }
    }
}
