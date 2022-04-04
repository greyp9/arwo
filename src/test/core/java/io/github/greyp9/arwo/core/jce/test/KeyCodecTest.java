package io.github.greyp9.arwo.core.jce.test;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.jce.AES;
import org.junit.Assert;
import org.junit.Test;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import java.security.GeneralSecurityException;
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
}
