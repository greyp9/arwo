package io.github.greyp9.arwo.core.security.realm;

public class AuthPrincipal {
    private final AppPrincipal principal;
    private final Object credential;

    public AuthPrincipal(final AppPrincipal principal, final Object credential) {
        this.principal = principal;
        this.credential = credential;
    }

    public final AppPrincipal getPrincipal() {
        return principal;
    }

    public final AppPrincipal authenticate(final Object credentialIn) {
        final boolean authenticated = (credentialIn == null) ?
                (this.credential == null) : credentialIn.equals(this.credential);
        return (authenticated ? principal : null);
    }
}
