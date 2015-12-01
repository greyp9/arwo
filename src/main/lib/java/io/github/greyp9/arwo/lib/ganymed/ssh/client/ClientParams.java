package io.github.greyp9.arwo.lib.ganymed.ssh.client;

public class ClientParams {
    private final String name;
    private final String user;
    private final String password;
    private final String privateKey;

    public final String getName() {
        return name;
    }

    public final String getUser() {
        return user;
    }

    public final String getPassword() {
        return password;
    }

    public final String getPrivateKey() {
        return privateKey;
    }

    public ClientParams(final String name, final String user, final String password, final String privateKey) {
        this.name = name;
        this.user = user;
        this.password = password;
        this.privateKey = privateKey;
    }
}
