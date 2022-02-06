package io.github.greyp9.arwo.core.ff.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.ff.GF256;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Random;
import java.util.logging.Logger;

@RunWith(Parameterized.class)
@SuppressWarnings({"checkstyle:magicnumber", "checkstyle:visibilitymodifier"})
public class GF256Test {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Parameterized.Parameters
    public static Object[][] data() {
        return new Object[][]{
                {2, UTF8Codec.toBytes("Hello"), 0 },
                {2, null, 256 },
                {3, UTF8Codec.toBytes("Hello world"), 0 },
                {3, null, 1024 },
                {4, UTF8Codec.toBytes("Hello world Hello world"), 0},
                {4, null, 4096 },
        };
    }

    @Parameterized.Parameter()
    public int shareCount;

    @Parameterized.Parameter(1)
    public byte[] inputFixed;

    @Parameterized.Parameter(2)
    public int inputLength;

    @Test
    public void testPermutations() {
        logger.finest(String.format("SHARE COUNT [%d]", shareCount));
        final byte[] input = (inputFixed == null) ? generate(inputLength) : inputFixed;
        final byte[][] output = new byte[shareCount][input.length];
        // replicate input into shares
        final Random random = new Random(0L);
        for (int i = 0; (i < input.length); ++i) {
            final byte[] generate = GF256.generate(random, shareCount - 1, input[i]);
            for (int j = 0; (j < shareCount); ++j) {
                output[j][i] = GF256.evaluate(generate, (byte) (j + 1));
            }
        }
        logger.finest(String.format("INPUT  [%s]", HexCodec.encode(input)));
        for (byte[] vector : output) {
            logger.finest(String.format("OUTPUT [%s]", HexCodec.encode(vector)));
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
        logger.finest(String.format("INPUT  [%s]", HexCodec.encode(inputRecover)));
        Assert.assertArrayEquals(input, inputRecover);
    }

    private static byte[] generate(final int length) {
        final byte[] input = new byte[length];
        for (int i = 0; (i < length); ++i) {
            input[i] = toByte(i);
        }
        return input;
    }

    private static final int LOW_BYTE_MASK = (1 << Byte.SIZE) - 1;

    private static byte toByte(final int i) {
        return (byte) (i & LOW_BYTE_MASK);
    }
}
