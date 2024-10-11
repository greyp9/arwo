package io.github.greyp9.arwo.lib.ganymed.ssh.server;

import io.github.greyp9.arwo.core.io.ByteU;

public class ServerParams {
    private final String name;
    private final String host;
    private final String algorithm;
    private final byte[] publicKey;

    public final String getName() {
        return name;
    }

    public final String getHost() {
        return host;
    }

    public final String getAlgorithm() {
        return algorithm;
    }

    public final byte[] getPublicKey() {
        return ByteU.copy(publicKey);
    }

    public ServerParams(final String name, final String host, final String algorithm, final byte[] publicKey) {
        this.name = name;
        this.host = host;
        this.algorithm = algorithm;
        this.publicKey = ByteU.copy(publicKey);
    }
}
