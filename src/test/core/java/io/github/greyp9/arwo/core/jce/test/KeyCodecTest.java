package io.github.greyp9.arwo.core.jce.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.jce.KeyU;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.core.XedU;
import io.github.greyp9.arwo.core.xed.transform.ProtectKeyTransform;
import io.github.greyp9.arwo.core.xed.transform.TransformContext;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.Assert;
import org.junit.Test;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.IOException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.SecureRandom;
import java.util.logging.Logger;

public class KeyCodecTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testDataCodec() throws GeneralSecurityException {
        final KeyGenerator keyGenerator = KeyGenerator.getInstance(AES.Const.ALGORITHM);
        keyGenerator.init(AES.Const.KEY_BITS);
        final SecretKey keyGenerate = keyGenerator.generateKey();
        final byte[] keyBytes = keyGenerate.getEncoded();
        Assert.assertEquals(AES.Const.KEY_BYTES, keyBytes.length);
        logger.finest(HexCodec.encode(keyBytes));
    }

    @Test
    public void testKeyLegacyCTR() throws GeneralSecurityException, IOException {
        final String secret = String.format("Basic %s", Base64Codec.encode(UTF8Codec.toBytes("foo:foo")));
        final byte[] salt = Base64Codec.decode("AAECAwQFBgc=");
        final int iterations = 1000;
        final int keySize = 128;
        final String pbe = "PBKDF2WithHmacSHA1";
        final String algorithm = "AES";
        final SecretKey key = KeyU.toKeyPBE(secret.toCharArray(), salt, iterations, keySize, pbe, algorithm);
        final KeyX keyX = new KeyX(key, KeyX.Const.TRANSFORM_CTR, KeyX.Const.PARAM_SPEC_IV);
        final String plainText = "plaintext";
        final String cryptText = keyX.protect(plainText);
        final String plainTextRecover = keyX.unprotect(cryptText);
        Assert.assertEquals(plainText, plainTextRecover);
    }

    @Test
    public void testKeyLegacyGCM() throws GeneralSecurityException, IOException {
        final String secret = String.format("Basic %s", Base64Codec.encode(UTF8Codec.toBytes("foo:foo")));
        final byte[] salt = Base64Codec.decode("AAECAwQFBgc=");
        final int iterations = 1000;
        final int keySize = 128;
        final String pbe = "PBKDF2WithHmacSHA1";
        final String algorithm = "AES";
        final SecretKey key = KeyU.toKeyPBE(secret.toCharArray(), salt, iterations, keySize, pbe, algorithm);
        final KeyX keyX = new KeyX(key, KeyX.Const.TRANSFORM_GCM, KeyX.Const.PARAM_SPEC_GCM);
        final String plainText = "plaintext";
        final String cryptText = keyX.protect(plainText);
        final String plainTextRecover = keyX.unprotect(cryptText);
        Assert.assertEquals(plainText, plainTextRecover);
    }

    @Test
    public void testCipherGCM() throws GeneralSecurityException {
        final KeyGenerator keyGenerator = KeyGenerator.getInstance(AES.Const.ALGORITHM);
        keyGenerator.init(AES.Const.KEY_BITS);
        final SecretKey key = keyGenerator.generateKey();

        final byte[] iv = KeyU.getRandomBytes(AES.Const.IV_BYTES_GCM, new SecureRandom());
        final GCMParameterSpec parameterSpec = new GCMParameterSpec(AES.Const.TAG_BYTES_GCM * Byte.SIZE, iv);
        final Cipher cipher = Cipher.getInstance(KeyX.Const.TRANSFORM_GCM);

        cipher.init(Cipher.ENCRYPT_MODE, key, parameterSpec);
        final byte[] plainText = UTF8Codec.toBytes("plainText");
        final byte[] cipherText = cipher.doFinal(plainText);
        Assert.assertEquals(plainText.length + AES.Const.TAG_BYTES_GCM, cipherText.length);
        cipher.init(Cipher.DECRYPT_MODE, key, parameterSpec);
        final byte[] plainTextRecover = cipher.doFinal(cipherText);
        Assert.assertArrayEquals(plainText, plainTextRecover);
    }

    @Test
    public void testParameters() throws IOException {
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_PROTECT);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeInstance typeGCM = xsdTypes.getElementType("{urn:arwo:protect}accountGCM");
        final TypeInstance typeUser = typeGCM.getInstance("user");
        final TypeInstance typePass = typeGCM.getInstance("password");

        final String user = "myuser";
        final String password = "mypassword";
        final NameTypeValues ntvIn = NameTypeValuesU.create(
                "accountGCM.accountTypeGCM.user", user,
                "accountGCM.accountTypeGCM.password", password);
        final ValueInstance valueInstance = ValueInstance.create(typeGCM, ntvIn);

        final SecretKeySpec secretKey = new SecretKeySpec(new byte[AES.Const.KEY_BYTES], AES.Const.ALGORITHM);
        final TransformContext transformContext = new TransformContext(secretKey, null);
        final ProtectKeyTransform protectKeyTransform = new ProtectKeyTransform(valueInstance, transformContext);
        final ValueInstance valueInstanceX = protectKeyTransform.transform();
        Assert.assertEquals(user, valueInstanceX.getNameTypeValue(typeUser).getValueS());

        final String passwordX = valueInstanceX.getNameTypeValue(typePass).getValueS();
        final KeyX keyX = new KeyX(secretKey,
                typePass.getDirective(XedU.TRANSFORM),
                typePass.getDirective(XedU.PARAM_SPEC));
        final String passwordRecover = keyX.unprotect(passwordX);
        Assert.assertEquals(password, passwordRecover);
    }
}
