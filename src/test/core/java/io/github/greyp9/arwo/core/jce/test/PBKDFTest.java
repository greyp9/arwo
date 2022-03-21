package io.github.greyp9.arwo.core.jce.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import java.security.GeneralSecurityException;
import java.security.spec.KeySpec;
import java.util.logging.Logger;

@RunWith(Parameterized.class)
@SuppressWarnings({"checkstyle:magicnumber", "checkstyle:visibilitymodifier"})
public class PBKDFTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    /**
     * https://www.ietf.org/rfc/rfc6070.txt
     */
    @Parameterized.Parameters
    public static Object[][] data() {
        return new Object[][]{
                {"password", "salt", 1, 20, "120fb6cffcf8b32c43e7225256c4f837a86548c9"},
                {"password", "salt", 2, 20, "ae4d0c95af6b46d32d0adff928f06dd02a303f8e"},
                {"password", "salt", 4_096, 20, "c5e478d59288c841aa530db6845c4c8d962893a0"},
                //{"password", "salt", 16_777_216, 20, "cf81c66fe8cfc04d1f31ecb65dab4089f7f179e8"},
                {"passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4_096, 25,
                        "348c89dbcbd32b2f32d814b8116e84cf2b17347ebc1800181c"},
                {"pass\0word", "sa\0lt", 4_096, 16, "89b69d0516f829893c696226650a8687"},
        };
    }

    @Parameterized.Parameter()
    public String password;

    @Parameterized.Parameter(1)
    public String salt;

    @Parameterized.Parameter(2)
    public int iterations;

    @Parameterized.Parameter(3)
    public int derivedKeyLength;

    @Parameterized.Parameter(4)
    public String keyHex;

    @Test
    public void testPBKDF() throws GeneralSecurityException {
        final SecretKeyFactory keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
        final KeySpec keySpec = new PBEKeySpec(
                password.toCharArray(), UTF8Codec.toBytes(salt), iterations, derivedKeyLength * Byte.SIZE);
        final SecretKey secretKey = keyFactory.generateSecret(keySpec);
        logger.finest(HexCodec.encode(secretKey.getEncoded()));
        Assert.assertEquals(keyHex, HexCodec.encode(secretKey.getEncoded()));
    }
}
