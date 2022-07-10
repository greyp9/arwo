package io.github.greyp9.arwo.core.jce.test;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.jce.KeyU;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.Assert;
import org.junit.Test;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.KeyStore;
import java.util.Random;

@SuppressWarnings("SpellCheckingInspection")
public class KeyStoreTest {

    @Test
    public void loadKeys() throws GeneralSecurityException, IOException {
        final Random random = new Random(0L);
        final byte[] keyBytes1 = KeyU.getRandomBytes(AES.Const.KEY_BYTES, random);
        final byte[] keyBytes2 = KeyU.getRandomBytes(AES.Const.KEY_BYTES, random);
        final String hex1 = "60b420bb3851d9d47acb933dbe70399bf6c92da33af01d4fb770e98c0325f41d";
        final String hex2 = "3ebaf8986da712c82bcd4d554bf0b54023c29b624de9ef9c2f931efc580f9afb";
        Assert.assertEquals(hex1, HexCodec.encode(keyBytes1));
        Assert.assertEquals(hex2, HexCodec.encode(keyBytes2));
        final SecretKey key1 = new SecretKeySpec(keyBytes1, AES.Const.ALGORITHM);
        final SecretKey key2 = new SecretKeySpec(keyBytes2, AES.Const.ALGORITHM);
        final char[] password = SystemU.userDir().toCharArray();
        final KeyStore.ProtectionParameter parameter = new KeyStore.PasswordProtection(password);

        final KeyStore keyStore = KeyStore.getInstance("PKCS12");
        keyStore.load(null, null);
        keyStore.setEntry("1", new KeyStore.SecretKeyEntry(key1), parameter);
        keyStore.setEntry("2", new KeyStore.SecretKeyEntry(key2), parameter);
        final Key key1Recover = keyStore.getKey("1", password);
        final Key key2Recover = keyStore.getKey("2", password);
        Assert.assertEquals(hex1, HexCodec.encode(key1Recover.getEncoded()));
        Assert.assertEquals(hex2, HexCodec.encode(key2Recover.getEncoded()));
    }
}
