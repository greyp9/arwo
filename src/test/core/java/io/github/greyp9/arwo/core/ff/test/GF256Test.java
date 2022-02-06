package io.github.greyp9.arwo.core.ff.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.ff.GF256;
import org.junit.Assert;
import org.junit.Test;

import java.util.Random;

public class GF256Test {

    @Test
    public void testSimpleValue() {
        final int shareCount = 2;
        final byte[] input = UTF8Codec.toBytes("Hello");
        final byte[][] output = new byte[shareCount][input.length];
        // replicate input into shares
        final Random random = new Random(0L);
        for (int i = 0; (i < input.length); ++i) {
            final byte[] generate = GF256.generate(random, shareCount - 1, input[i]);
            for (int j = 0; (j < shareCount); ++j) {
                output[j][i] = GF256.evaluate(generate, (byte) (j + 1));
            }
        }
        // reconstitute input from shares
        final byte[] inputRecover = new byte[input.length];
        for (int i = 0; (i < input.length); ++i) {
            final byte[][] parts = new byte[shareCount][2];
            for (int j = 0; (j < shareCount); ++j) {
                parts[j][0] = (byte) (j + 1);
                parts[j][1] = output[j][i];
            }
            inputRecover[i] = GF256.recover(parts);
        }
        Assert.assertArrayEquals(input, inputRecover);
    }

    @Test
    public void testLongValue() {
        final int shareCount = 2;
        final int sizeInput = 4096;
        final byte[] input = new byte[sizeInput];
        for (int i = 0; (i < input.length); ++i) {
            input[i] = toByte(i);
        }
        final byte[][] output = new byte[shareCount][input.length];
        // replicate input into shares
        final Random random = new Random(0L);
        for (int i = 0; (i < input.length); ++i) {
            final byte[] generate = GF256.generate(random, shareCount - 1, input[i]);
            for (int j = 0; (j < shareCount); ++j) {
                output[j][i] = GF256.evaluate(generate, (byte) (j + 1));
            }
        }
        // reconstitute input from shares
        final byte[] inputRecover = new byte[input.length];
        for (int i = 0; (i < input.length); ++i) {
            final byte[][] parts = new byte[shareCount][2];
            for (int j = 0; (j < shareCount); ++j) {
                parts[j][0] = (byte) (j + 1);
                parts[j][1] = output[j][i];
            }
            inputRecover[i] = GF256.recover(parts);
        }
        Assert.assertArrayEquals(input, inputRecover);
    }

    private static final int LOW_BYTE_MASK = (1 << Byte.SIZE) - 1;

    private static byte toByte(final int i) {
        return (byte) (i & LOW_BYTE_MASK);
    }
}
