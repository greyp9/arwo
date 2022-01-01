package io.github.greyp9.arwo.core.codec.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.util.logging.Logger;

public class Base64CodecTest {

    @Test
    public void testCodec_Simple() throws IOException {
        final String plaintext = "Hello world!";
        final String encoded = Base64Codec.encode(UTF8Codec.toBytes(plaintext));
        Assert.assertEquals("SGVsbG8gd29ybGQh", encoded);
        final String plaintextRecover = UTF8Codec.toString(Base64Codec.decode(encoded));
        Assert.assertEquals(plaintext, plaintextRecover);
    }

    @Test
    public void testCodec_Permute() throws IOException {
        final byte[] input = new byte[512];
        for (int i = 0; (i < input.length); ++i) {
            input[i] = (byte) (i % 256);
        }
        final String encoded = Base64Codec.encode(input);
        Logger.getLogger(getClass().getName()).info(encoded);
        Assert.assertTrue(encoded.startsWith("AAECAwQFBgcI"));
        Assert.assertTrue(encoded.endsWith("+Pn6+/z9/v8="));
        final byte[] inputRecover = Base64Codec.decode(encoded);
        Assert.assertArrayEquals(input, inputRecover);
    }

    @Test
    public void testCodec_BadInput() {
        final String encoded = "0123456789abcdefgh!@#$0123456789";
        Assert.assertThrows(IllegalArgumentException.class, () -> Base64Codec.decode(encoded));
    }
}
