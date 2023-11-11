package io.github.greyp9.arwo.core.ff.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.ff.GF256;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Random;
import java.util.logging.Logger;
import java.util.stream.Stream;

@SuppressWarnings({"checkstyle:magicnumber", "checkstyle:visibilitymodifier"})
public class GF256Test {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public static Stream<Arguments> data() {
        return Stream.of(
                Arguments.arguments(2, 2, UTF8Codec.toBytes("Hello"), 0),
                Arguments.arguments(2, 2, null, 256),
                Arguments.arguments(3, 3, UTF8Codec.toBytes("Hello world"), 0),
                Arguments.arguments(3, 3, null, 1024),
                Arguments.arguments(4, 4, UTF8Codec.toBytes("Hello world Hello world"), 0),
                Arguments.arguments(4, 4, null, 4096),

                Arguments.arguments(3, 2, UTF8Codec.toBytes("Hello"), 0),
                Arguments.arguments(3, 2, null, 1024),
                Arguments.arguments(4, 2, UTF8Codec.toBytes("Hello"), 0),
                Arguments.arguments(4, 2, null, 1024)
        );
    }

    @ParameterizedTest
    @MethodSource("data")
    public final void testPermutations(final int sharesGenerated, final int sharesNeeded,
                                 final byte[] inputFixed, final int inputLength) {
        logger.finest(String.format("SHARES GENERATED [%d], NEEDED [%d]", sharesGenerated, sharesNeeded));
        final byte[] input = (inputFixed == null) ? generate(inputLength) : inputFixed;
        final byte[][] output = new byte[sharesGenerated][input.length];
        // replicate input into shares
        final Random random = new Random(0L);
        for (int i = 0; (i < input.length); ++i) {
            final byte[] generate = GF256.generate(random, sharesNeeded - 1, input[i]);
            for (int j = 0; (j < sharesGenerated); ++j) {
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
            final byte[][] parts = new byte[sharesNeeded][2];
            for (int j = 0; (j < sharesNeeded); ++j) {
                parts[j][0] = (byte) (j + 1);
                parts[j][1] = output[j][i];
            }
            inputRecover[i] = GF256.recover(parts);
        }
        logger.finest(String.format("INPUT  [%s]", HexCodec.encode(inputRecover)));
        Assertions.assertArrayEquals(input, inputRecover);
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
