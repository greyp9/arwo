package io.github.greyp9.arwo.core.jce.test;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.value.Value;
import org.junit.Assert;
import org.junit.Test;

import javax.crypto.Mac;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.security.GeneralSecurityException;
import java.util.logging.Logger;

public class MacTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testAlgorithmHmacSHA256() throws GeneralSecurityException {
        // fabricate some bytes
        final byte[] clearBytes = new byte[AES.Const.KEY_BYTES];
        logger.finest(HexCodec.encode(clearBytes));
        // derive hash
        final byte[] hashBytes = HashU.sha256(clearBytes);
        Assert.assertEquals(
                "66687aadf862bd776c8fc18b8e9f8e20089714856ee233b3902a591d0d5f2925",
                HexCodec.encode(hashBytes));
        // fabricate a key
        final byte[] keyBytes = new byte[AES.Const.KEY_BYTES];
        logger.finest(HexCodec.encode(keyBytes));
        final SecretKey key = new SecretKeySpec(keyBytes, 0, keyBytes.length, AES.Const.ALGORITHM);
        // derive the Message Authentication Code
        final Mac mac = Mac.getInstance("HmacSHA256");
        mac.init(key);
        final byte[] macBytes = mac.doFinal(clearBytes);
        Assert.assertEquals(
                "33ad0a1c607ec03b09e6cd9893680ce210adf300aa1f2660e1b22e10f170f92a",
                HexCodec.encode(macBytes));
        // mac verify failure
        final Mac macFail = Mac.getInstance("HmacSHA256");
        final byte[] keyBytes2 = HexCodec.decode(Value.generate("01", clearBytes.length));
        final SecretKey key2 = new SecretKeySpec(keyBytes2, 0, keyBytes2.length, AES.Const.ALGORITHM);
        macFail.init(key2);
        final byte[] macBytes2 = macFail.doFinal(clearBytes);
        Assert.assertNotEquals(HexCodec.encode(macBytes), HexCodec.encode(macBytes2));
        // verify the mac
        final Mac macVerify = Mac.getInstance("HmacSHA256");
        macVerify.init(key);
        final byte[] macBytesVerify = macVerify.doFinal(clearBytes);
        Assert.assertArrayEquals(macBytes, macBytesVerify);
    }
}
