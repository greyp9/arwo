package io.github.greyp9.arwo.core.security.realm.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.security.realm.AppRealm;
import io.github.greyp9.arwo.core.security.realm.AuthPrincipal;
import io.github.greyp9.arwo.core.security.update.AppRealmFactory;
import junit.framework.TestCase;
import org.junit.Assert;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class AppRealmTest extends TestCase {

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(java.util.logging.Logger.getLogger(""));
    }

    public void testHashCredential() throws Exception {
        final String salt = "arwo-salt", credential = "arwo-credential";
        final String credentialHash = AppRealm.hashCredential(salt, credential);
        Assert.assertEquals("expectedHash", "lSi8VXKeMAEv7Qd41GpW5oh5Gfwkq1qpKUB6kn/NARk=", credentialHash);
    }

    public void testAppRealmFromScratch() throws Exception {
        // setup realm
        final String realmName = getClass().getSimpleName();
        final String salt = "arwo-salt";
        final Collection<AuthPrincipal> principals = new ArrayList<AuthPrincipal>();
        final String name = "arwo-name", credential = "arwo-credential";
        final String credentialHash = AppRealm.hashCredential(salt, credential);
        principals.add(new AuthPrincipal(new AppPrincipal(name, Collections.singleton("*")), credentialHash));
        final AppRealm appRealm = new AppRealm(realmName, salt, principals);
        // authenticate
        final AppPrincipal appPrincipal1 = appRealm.authenticate(null, null);
        Assert.assertNull("user should not authenticate", appPrincipal1);
        // authenticate
        final AppPrincipal appPrincipal2 = appRealm.authenticate(name, null);
        Assert.assertNull("user should not authenticate", appPrincipal2);
        // authenticate
        final AppPrincipal appPrincipal3 = appRealm.authenticate(credential, name);
        Assert.assertNull("user should not authenticate", appPrincipal3);
        // authenticate
        final AppPrincipal appPrincipal4 = appRealm.authenticate(name, credential);
        Assert.assertNotNull("user should authenticate", appPrincipal4);
        final boolean userInRoleStar = appRealm.isUserInRole(appPrincipal4, "*");
        Assert.assertTrue("user should authorize", userInRoleStar);
        final boolean userInRoleRead = appRealm.isUserInRole(appPrincipal4, "read");
        Assert.assertTrue("user should authorize", userInRoleRead);
    }

    public void testAppRealmFromDocument() throws Exception {
        // realm schema
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final byte[] xsdRealm = StreamU.read(urlInitial);
        Assert.assertEquals(1321, xsdRealm.length);
        Assert.assertEquals("25b6f2fe", CRCU.crc32String(xsdRealm));
        // realm document
        final byte[] xmlRealm = StreamU.read(ResourceU.resolve(AppRealmTest.Const.XML));
        Assert.assertEquals(285, xmlRealm.length);
        Assert.assertEquals("bc1608a7", CRCU.crc32String(xmlRealm));
        // realm
        final AppRealm appRealm = AppRealmFactory.toAppRealm(xsdRealm, xmlRealm);
        Assert.assertNotNull(appRealm);
        // authenticate
        final String name = "arwo-name", credential = "arwo-credential";
        // authenticate
        final AppPrincipal appPrincipal1 = appRealm.authenticate(null, null);
        Assert.assertNull("user should not authenticate", appPrincipal1);
        // authenticate
        final AppPrincipal appPrincipal2 = appRealm.authenticate(name, null);
        Assert.assertNull("user should not authenticate", appPrincipal2);
        // authenticate
        final AppPrincipal appPrincipal3 = appRealm.authenticate(credential, name);
        Assert.assertNull("user should not authenticate", appPrincipal3);
        // authenticate
        final AppPrincipal appPrincipal4 = appRealm.authenticate(name, credential);
        Assert.assertNotNull("user should authenticate", appPrincipal4);
        final boolean userInRoleStar = appRealm.isUserInRole(appPrincipal4, "*");
        Assert.assertTrue("user should authorize", userInRoleStar);
        final boolean userInRoleRead = appRealm.isUserInRole(appPrincipal4, "read");
        Assert.assertTrue("user should authorize", userInRoleRead);
    }

    public static class Const {
        private static final String XML = "io/github/greyp9/arwo/xml/realm/realm.xml";
    }
}
