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
    private final String salt;
    private final Map<String, AuthPrincipal> principals;
    private final String defaultPassword;
    private final Date timestamp;

    public AppRealm(final String realmName, final String salt, final Collection<AuthPrincipal> principals) {
        this.realmName = realmName;
        this.salt = salt;
        this.principals = new TreeMap<String, AuthPrincipal>();
        for (final AuthPrincipal principal : principals) {
            this.principals.put(principal.getPrincipal().getName(), principal);
        }
        defaultPassword = generatePassword(new SecureRandom());
        Logger.getLogger(getClass().getName()).log(Level.OFF, String.format("[%s][%s]", realmName, defaultPassword));
        //System.out.println(defaultPassword);
        this.timestamp = new Date();
    }

    public final String getName() {
        return realmName;
    }

    public final String getSalt() {
        return salt;
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
        // translate request into a principal to test
        final AppPrincipal principalIn = ((name == null) ? null : new AppPrincipal(name, false, null));
        final String credentialIn = hashCredential(salt, credential);
        // find the resident principal matching the request
        AuthPrincipal principalAuth;
        if (principals.isEmpty()) {
            final AppPrincipal principal = new AppPrincipal(Const.PRINCIPAL_NAME, true,
                    Collections.singletonList(Const.ROLE_WILDCARD));
            principalAuth = new AuthPrincipal(principal, hashCredential(salt, defaultPassword));
        } else if (name == null) {
            principalAuth = null;
        } else {
            principalAuth = principals.get(name);
        }
        // test expected credential against actual credential
        final AppPrincipal principal = ((principalAuth == null) ? null : principalAuth.authenticate(credentialIn));
        return (principal == null) ? principalIn : copy(principal);
    }

    @SuppressWarnings("PMD.UseVarargs")
    private AppPrincipal authenticate(final X509Certificate[] certificates) {
        final X509Certificate certificate = certificates[0];
        final String pubkeyBase64 = Base64Codec.encode(certificate.getPublicKey().getEncoded());
        return new AppPrincipal(certificate.getSubjectDN().getName(), false,
                Arrays.asList(Const.ROLE_WILDCARD, pubkeyBase64));
    }

    public static String hashCredential(final String salt, final String credential) {
        return Base64Codec.encode(HashU.sha256((credential + salt).getBytes(Charset.forName("UTF-8"))));  // i18n in
    }

    private AppPrincipal copy(final AppPrincipal principal) {
        return new AppPrincipal(principal.getName(), principal.isAuthenticated(), principal.getRoles());
    }

    private String generatePassword(final SecureRandom secureRandom) {
        final byte[] bytes = new byte[Const.SIZE_GEN_PASS];
        secureRandom.nextBytes(bytes);
        return Base64Codec.encode(bytes);
        //return PRINCIPAL_NAME;
    }

    private static class Const {
        private static final String PRINCIPAL_NAME = "arwo";  // i18n internal
        private static final String ROLE_WILDCARD = "*";  // i18n internal
        private static final int SIZE_GEN_PASS = 3;
    }
}
