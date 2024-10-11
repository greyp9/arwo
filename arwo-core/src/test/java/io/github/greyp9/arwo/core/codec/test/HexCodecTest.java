package io.github.greyp9.arwo.core.codec.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HexCodecTest {

    @Test
    public void testCodec_Simple() {
        final String plaintext = "Hello world!";
        final String encoded = HexCodec.encode(UTF8Codec.toBytes(plaintext));
        Assertions.assertEquals("48656c6c6f20776f726c6421", encoded);
        final String plaintextRecover = UTF8Codec.toString(HexCodec.decode(encoded));
        Assertions.assertEquals(plaintext, plaintextRecover);
    }

    @Test
    public void testCodec_Permute() {
        final int permuteChar = 256;
        final byte[] input = new byte[permuteChar * 2];
        for (int i = 0; (i < input.length); ++i) {
            input[i] = (byte) (i % permuteChar);
        }
        final String encoded = HexCodec.encode(input);
        Assertions.assertTrue(encoded.startsWith("000102030405"));
        Assertions.assertTrue(encoded.endsWith("fafbfcfdfeff"));
        final byte[] inputRecover = HexCodec.decode(encoded);
        Assertions.assertArrayEquals(input, inputRecover);
    }

    @Test
    public void testCodec_BadInput() {
        final String encoded = "0123456789abcdefghij0123456789";
        final byte[] plain = HexCodec.decode(encoded);
        final String encodedRecover = HexCodec.encode(plain);
        Assertions.assertNotEquals(encoded, encodedRecover);
    }
}
