package io.github.greyp9.arwo.s3.connection;

import software.amazon.awssdk.services.s3.S3Client;

public final class S3Connection {
    private final S3Client s3Client;

    public S3Client getS3Client() {
        return s3Client;
    }

    public S3Connection(final S3Client s3Client) {
        this.s3Client = s3Client;
    }
}
