package io.github.greyp9.arwo.core.security.realm;

import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.hash.secure.HashU;

import java.nio.charset.Charset;
import java.security.Principal;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

public class AppRealm {
    private final String realmName;
    private final Map<String, AuthPrincipal> principals;
    private final String defaultPassword;
    private final Date timestamp;

    public AppRealm(final String realmName, final Collection<AuthPrincipal> principals) {
        this.realmName = realmName;
        this.principals = new TreeMap<String, AuthPrincipal>();
        for (final AuthPrincipal principal : principals) {
            this.principals.put(principal.getPrincipal().getName(), principal);
        }
        defaultPassword = generatePassword();
        Logger.getLogger(getClass().getName()).log(Level.OFF, String.format("[%s][%s]", realmName, defaultPassword));
        //System.out.println(defaultPassword);
        this.timestamp = new Date();
    }

    public final String getName() {
        return realmName;
    }

    public final Date getTimestamp() {
        return new Date(timestamp.getTime());
    }

    public final void clear() {
        principals.clear();
    }

    public final int getSize() {
        return principals.size();
    }

    public final boolean isUserInRole(final Principal user, final String role) {
        boolean isUserInRole = false;
        if (user instanceof AppPrincipal) {
            final AppPrincipal principal = (AppPrincipal) user;
            final Collection<String> userRoles = principal.getRoles();
            final boolean hasRoot = userRoles.contains(Const.ROLE_WILDCARD);
            final boolean hasRole = userRoles.contains(role);
            isUserInRole = (hasRoot || hasRole);
        }
        return isUserInRole;
    }

    public final AppPrincipal authenticate(final String name, final Object credential) {
        AppPrincipal appPrincipal = null;
        if (credential instanceof String) {
            appPrincipal = authenticate(name, (String) credential);
        } else if (credential instanceof X509Certificate[]) {
            appPrincipal = authenticate((X509Certificate[]) credential);
        }
        return appPrincipal;
    }

    private AppPrincipal authenticate(final String name, final String credential) {
        AuthPrincipal authPrincipal;
        if (principals.isEmpty()) {
            final AppPrincipal principal = new AppPrincipal(Const.PRINCIPAL_NAME,
                    Collections.singletonList(Const.ROLE_WILDCARD));
            authPrincipal = new AuthPrincipal(principal, hashCredential(Const.PRINCIPAL_NAME, defaultPassword));
        } else {
            authPrincipal = principals.get(name);
        }
        final String credentialHash = hashCredential(name, credential);
        final AppPrincipal principal = ((authPrincipal == null) ? null : authPrincipal.authenticate(credentialHash));
        return (principal == null) ? null : copy(principal);
    }

    @SuppressWarnings("PMD.UseVarargs")
    private AppPrincipal authenticate(final X509Certificate[] certificates) {
        final X509Certificate certificate = certificates[0];
        final String pubkeyBase64 = Base64Codec.encode(certificate.getPublicKey().getEncoded());
        return new AppPrincipal(certificate.getSubjectDN().getName(),
                Arrays.asList(Const.ROLE_WILDCARD, pubkeyBase64));
    }

    private String hashCredential(final String name, final String credential) {
        return Base64Codec.encode(HashU.sha256((credential + name).getBytes(Charset.forName("UTF-8"))));  // i18n
    }

    private AppPrincipal copy(final AppPrincipal principal) {
        return new AppPrincipal(principal.getName(), principal.getRoles());
    }

    private String generatePassword() {
        final byte[] bytes = new byte[Const.SIZE_GEN_PASS];
        new SecureRandom().nextBytes(bytes);
        return Base64Codec.encode(bytes);
        //return PRINCIPAL_NAME;
    }

    private static class Const {
        private static final String PRINCIPAL_NAME = "arwo";  // i18n
        private static final String ROLE_WILDCARD = "*";  // i18n
        private static final int SIZE_GEN_PASS = 3;
    }
}
