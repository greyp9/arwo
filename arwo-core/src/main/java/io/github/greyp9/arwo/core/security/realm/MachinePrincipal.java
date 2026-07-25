package io.github.greyp9.arwo.core.security.realm;

public class MachinePrincipal extends AuthPrincipal {
    private final AuthPrincipal authPrincipal;
    private final Object credentialGenerate;

    public MachinePrincipal(final AppPrincipal principal, final Object credential, final Object credentialGenerate) {
        super(principal, credential);
        this.authPrincipal = new AuthPrincipal(principal, credential);
        this.credentialGenerate = credentialGenerate;
    }

    public final AuthPrincipal getAuthPrincipal() {
        return authPrincipal;
    }

    public final Object getCredentialGenerate() {
        return credentialGenerate;
    }
}
