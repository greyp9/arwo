package io.github.greyp9.arwo.core.security.realm.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.security.realm.AppRealm;
import io.github.greyp9.arwo.core.security.realm.AuthPrincipal;
import io.github.greyp9.arwo.core.security.update.AppRealmFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class AppRealmTest {

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(java.util.logging.Logger.getLogger(""));
    }

    @Test
    public void testHashCredential() throws Exception {
        final String salt = "arwo-salt";
        final String credential = "arwo-credential";
        final String credentialHash = AppRealm.hashCredential(salt, credential);
        Assertions.assertEquals("lSi8VXKeMAEv7Qd41GpW5oh5Gfwkq1qpKUB6kn/NARk=", credentialHash, "expectedHash");
    }

    @Test
    public void testAppRealmFromScratch() throws Exception {
        // setup realm
        final String realmName = getClass().getSimpleName();
        final String salt = "arwo-salt";
        final Collection<AuthPrincipal> principals = new ArrayList<AuthPrincipal>();
        final String name = "arwo-name";
        final String credential = "arwo-credential";
        final String credentialHash = AppRealm.hashCredential(salt, credential);
        principals.add(new AuthPrincipal(new AppPrincipal(name, Collections.singleton("*")), credentialHash));
        final AppRealm appRealm = new AppRealm(realmName, salt, principals);
        // authenticate
        final AppPrincipal appPrincipal1 = appRealm.authenticate(null, null);
        Assertions.assertNull(appPrincipal1, "user should not authenticate");
        // authenticate
        final AppPrincipal appPrincipal2 = appRealm.authenticate(name, null);
        Assertions.assertNull(appPrincipal2, "user should not authenticate");
        // authenticate
        final AppPrincipal appPrincipal3 = appRealm.authenticate(credential, name);
        Assertions.assertFalse(appPrincipal3.isAuthenticated(), "user should not authenticate");
        Assertions.assertEquals(0, appPrincipal3.getRoles().size());
        // authenticate
        final AppPrincipal appPrincipal4 = appRealm.authenticate(name, credential);
        Assertions.assertNotNull(appPrincipal4, "user should authenticate");
        final boolean userInRoleStar = appRealm.isUserInRole(appPrincipal4, "*");
        Assertions.assertTrue(userInRoleStar, "user should authorize");
        final boolean userInRoleRead = appRealm.isUserInRole(appPrincipal4, "read");
        Assertions.assertTrue(userInRoleRead, "user should authorize");
    }

    @SuppressWarnings("checkstyle:magicnumber")
    @Test
    public void testAppRealmFromDocument() throws Exception {
        // realm schema
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final byte[] xsdRealm = StreamU.read(urlInitial);
        Assertions.assertEquals(1321, xsdRealm.length);
        Assertions.assertEquals("25b6f2fe", CRCU.crc32String(xsdRealm));
        // realm document
        final byte[] xmlRealm = StreamU.read(ResourceU.resolve(AppRealmTest.Const.XML));
        Assertions.assertEquals(285, xmlRealm.length);
        Assertions.assertEquals("bc1608a7", CRCU.crc32String(xmlRealm));
        // realm
        final AppRealm appRealm = AppRealmFactory.toAppRealm(xsdRealm, xmlRealm);
        Assertions.assertNotNull(appRealm);
        // authenticate
        final String name = "arwo-name";
        final String credential = "arwo-credential";
        // authenticate
        final AppPrincipal appPrincipal1 = appRealm.authenticate(null, null);
        Assertions.assertNull(appPrincipal1, "user should not authenticate");
        // authenticate
        final AppPrincipal appPrincipal2 = appRealm.authenticate(name, null);
        Assertions.assertNull(appPrincipal2, "user should not authenticate");
        // authenticate
        final AppPrincipal appPrincipal3 = appRealm.authenticate(credential, name);
        Assertions.assertFalse(appPrincipal3.isAuthenticated(), "user should not authenticate");
        Assertions.assertEquals(0, appPrincipal3.getRoles().size());
        // authenticate
        final AppPrincipal appPrincipal4 = appRealm.authenticate(name, credential);
        Assertions.assertNotNull(appPrincipal4, "user should authenticate");
        final boolean userInRoleStar = appRealm.isUserInRole(appPrincipal4, "*");
        Assertions.assertTrue(userInRoleStar, "user should authorize");
        final boolean userInRoleRead = appRealm.isUserInRole(appPrincipal4, "read");
        Assertions.assertTrue(userInRoleRead, "user should authorize");
    }

    public static class Const {
        private static final String XML = "io/github/greyp9/arwo/xml/realm/realm.xml";
    }
}
