package io.github.greyp9.arwo.s3.connection;

import io.github.greyp9.arwo.core.connect.ConnectionFactory;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.S3ClientBuilder;

import java.io.IOException;

public class S3ConnectionFactory implements ConnectionFactory {
    private final String region;
    private final String bucket;

    public S3ConnectionFactory(final String region, final String bucket) {
        this.region = region;
        this.bucket = bucket;
    }

    @Override
    public final ConnectionResource create(final String name) throws IOException {
        final S3ClientBuilder s3ClientBuilder = S3Client.builder()
                .credentialsProvider(DefaultCredentialsProvider.builder().build())  // [~/.aws/credentials]?
                .region(Region.of(region));
        return new S3ConnectionResource(region, bucket, new S3Connection(s3ClientBuilder.build()));
    }
}
