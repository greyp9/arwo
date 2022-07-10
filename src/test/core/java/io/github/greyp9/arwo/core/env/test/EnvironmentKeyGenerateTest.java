package io.github.greyp9.arwo.core.env.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.ff.DataSplitter;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.jce.KeyCodec;
import io.github.greyp9.arwo.core.jce.KeyU;
import io.github.greyp9.arwo.core.jce.KeyX;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;
import java.util.Random;
import java.util.logging.Logger;

public class EnvironmentKeyGenerateTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        logger.finest("setup()");
    }

    @Test
    public void testGenerateKey() throws NoSuchAlgorithmException {
        final KeyGenerator keyGenerator = KeyGenerator.getInstance(AES.Const.ALGORITHM);
        keyGenerator.init(AES.Const.KEY_BITS);
        final SecretKey key = keyGenerator.generateKey();
        final byte[] keyBytes = key.getEncoded();
        Assert.assertEquals(AES.Const.KEY_BYTES, keyBytes.length);
    }

    @Test
    public void testGenerateRecoverSecret() throws GeneralSecurityException {
        // generate the secret to be protected
        final KeyGenerator keyGenerator = KeyGenerator.getInstance(AES.Const.ALGORITHM);
        keyGenerator.init(AES.Const.KEY_BITS);
        final SecretKey keyGenerate = keyGenerator.generateKey();
        final byte[] keyBytes = keyGenerate.getEncoded();
        logger.finest(HexCodec.encode(keyBytes));
        // split the secret
        final int shareCount = 3;
        final int thresholdCount = 3;
        final Random random = new Random(0L);
        final DataSplitter dataSplitter = new DataSplitter(shareCount, thresholdCount, random);
        final byte[][] shares = dataSplitter.split(keyBytes);
        // protect the shares
        final String[] pbkdfData = {
                "abcdefghijklmnopqrst",
                "01234567890123456789",
                "abcdefghijklmnopqrst01234567890123456789",
        };
        Assert.assertEquals(shareCount, pbkdfData.length);
        final byte[][] sharesWrapped = new byte[pbkdfData.length][];
        for (int i = 0; (i < shareCount); ++i) {
            final String password = pbkdfData[i];
            final byte[] salt = HashU.sha256(UTF8Codec.toBytes(password));
            final SecretKey key = KeyU.toKeyPBE(password.toCharArray(), salt);
            final KeyCodec keyCodec = new KeyCodec(key, KeyX.Const.TRANSFORM_CTR, KeyX.Const.PARAM_SPEC_IV, random);
            final byte[] shareWrapped = keyCodec.encode(shares[i]);
            logger.finest(HexCodec.encode(shareWrapped));
            sharesWrapped[i] = shareWrapped;
            Assert.assertEquals(shares[i].length + AES.Const.IV_BYTES_CTR, sharesWrapped[i].length);
        }
        // recover the shares
        final byte[][] sharesUnwrapped = new byte[pbkdfData.length][];
        for (int i = 0; (i < shareCount); ++i) {
            final String password = pbkdfData[i];
            final byte[] salt = HashU.sha256(UTF8Codec.toBytes(password));
            final SecretKey key = KeyU.toKeyPBE(password.toCharArray(), salt);
            final KeyCodec keyCodec = new KeyCodec(key, KeyX.Const.TRANSFORM_CTR, KeyX.Const.PARAM_SPEC_IV, random);
            final byte[] shareUnwrapped = keyCodec.decode(sharesWrapped[i]);
            sharesUnwrapped[i] = shareUnwrapped;
            Assert.assertEquals(shares[i].length, sharesUnwrapped[i].length);
        }
        // reconstitute the secret
        final DataSplitter dataSplitterRecover = new DataSplitter(shareCount, thresholdCount, null);
        final byte[] keyBytesRecover = dataSplitterRecover.join(sharesUnwrapped);
        logger.finest(HexCodec.encode(keyBytesRecover));
        Assert.assertArrayEquals(keyBytes, keyBytesRecover);
    }
}
