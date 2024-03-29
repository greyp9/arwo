package io.github.greyp9.arwo.core.xsd.value.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.jce.KeyU;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.transform.ProtectKeyTransform;
import io.github.greyp9.arwo.core.xed.transform.TransformContext;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.logging.Logger;

public class ValueInstanceProtectTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testTransformProtect() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_PROTECT);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:protect}accountPBE");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // validate
        final Collection<String> messages0 = xed.validate();
        Assertions.assertEquals("[]", messages0.toString());
        // navigate in document
        final XedCursor cursorAccount = new XedNav(xed).getRoot();
        final TypeInstance instanceAccount = cursorAccount.getTypeInstance();
        Assertions.assertEquals(qname, instanceAccount.getQName());
        // check
        final NameTypeValues ntv = NameTypeValuesU.create(
                "accountPBE.accountTypePBE.user", "user",
                "accountPBE.accountTypePBE.password", "password");
        final ValueInstance valueInstance = ValueInstance.create(instanceAccount, ntv);
        Assertions.assertEquals(2, valueInstance.getNameTypeValues().size());
        // update value instance
        final char[] secret = "secret".toCharArray();
        final byte[] salt = Base64Codec.decode("AAECAwQFBgc=");
        final SecretKey keyEncrypt = KeyU.toKeyPBE(secret, salt, 1000, 128, "PBKDF2WithHmacSHA1", "AES");
        final TransformContext context = new TransformContext(keyEncrypt, null);
        final ValueInstance valueInstanceX = new ProtectKeyTransform(valueInstance, context).transform();
        // check
        Assertions.assertEquals(2, valueInstanceX.getNameTypeValues().size());
        final NameTypeValues ntvX = valueInstanceX.getNameTypeValues();
        Assertions.assertEquals("user", ntvX.getValue("accountPBE.accountTypePBE.user"));
        // check
        final String valueProtect = ntvX.getValue("accountPBE.accountTypePBE.password");
        final SecretKey keyDecrypt = KeyU.toKeyPBE(secret, salt, 1000, 128, "PBKDF2WithHmacSHA1", "AES");
        final KeyX keyX = new KeyX(keyDecrypt, "AES/CBC/PKCS5Padding", IvParameterSpec.class.getSimpleName());
        final String plaintext = keyX.unprotect(valueProtect);
        Assertions.assertEquals("password", plaintext);
    }

    @Test
    public void testProtect() throws Exception {
        final char[] secret = "secret".toCharArray();
        final byte[] salt = Base64Codec.decode("AAECAwQFBgc=");
        final SecretKey key = KeyU.toKeyPBE(secret, salt, 1000, 128, "PBKDF2WithHmacSHA1", "AES");
        final KeyX keyX = new KeyX(key, "AES/CBC/PKCS5Padding", KeyX.Const.PARAM_SPEC_IV);
        final String protect1 = keyX.protect("toProtect");
        final String protect2 = keyX.protect("toProtect");
        Assertions.assertNotEquals(protect1, protect2);
        Assertions.assertEquals("toProtect", keyX.unprotect(protect1));
        Assertions.assertEquals("toProtect", keyX.unprotect(protect2));
    }
}
