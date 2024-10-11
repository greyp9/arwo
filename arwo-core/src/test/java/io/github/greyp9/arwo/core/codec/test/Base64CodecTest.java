package io.github.greyp9.arwo.core.codec.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.logging.Logger;

public class Base64CodecTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testCodec_Simple() throws IOException {
        final String plaintext = "Hello world!";
        final String encoded = Base64Codec.encode(UTF8Codec.toBytes(plaintext));
        Assertions.assertEquals("SGVsbG8gd29ybGQh", encoded);
        final String plaintextRecover = UTF8Codec.toString(Base64Codec.decode(encoded));
        Assertions.assertEquals(plaintext, plaintextRecover);
    }

    @Test
    public void testCodec_Resource() throws IOException {
        final byte[] bytes = StreamU.read(ResourceU.resolve("io/github/greyp9/arwo/core/codec/b64.txt"));
        final String string = UTF8Codec.toString(bytes);
        final String plaintextRecover = UTF8Codec.toString(Base64Codec.decode(string));
        logger.info(plaintextRecover);
    }

    @Test
    public void testCodec_Permute() throws IOException {
        final int permuteChar = 256;
        final byte[] input = new byte[permuteChar * 2];
        for (int i = 0; (i < input.length); ++i) {
            input[i] = (byte) (i % permuteChar);
        }
        final String encoded = Base64Codec.encode(input);
        logger.finest(encoded);
        Assertions.assertTrue(encoded.startsWith("AAECAwQFBgcI"));
        Assertions.assertTrue(encoded.endsWith("+Pn6+/z9/v8="));
        final byte[] inputRecover = Base64Codec.decode(encoded);
        Assertions.assertArrayEquals(input, inputRecover);
    }

    @Test
    public void testCodec_BadInput() {
        final String encoded = "0123456789abcdefgh!@#$0123456789";
        Assertions.assertThrows(IllegalArgumentException.class, () -> Base64Codec.decode(encoded));
    }
}
