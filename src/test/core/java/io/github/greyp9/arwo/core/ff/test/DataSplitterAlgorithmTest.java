package io.github.greyp9.arwo.core.ff.test;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.ff.GF256;
import io.github.greyp9.arwo.core.value.Value;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Random;
import java.util.logging.Logger;
import java.util.stream.Stream;

@SuppressWarnings({ "checkstyle:magicnumber", "checkstyle:visibilitymodifier" })
public class DataSplitterAlgorithmTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public static Stream<Arguments> data() {
        return Stream.of(
                Arguments.arguments(2, 2, Value.generate("00", 32)),
                Arguments.arguments(3, 3, Value.generate("01", 32)),

                Arguments.arguments(3, 2, Value.generate("02", 32)),
                Arguments.arguments(4, 3, Value.generate("03", 32)),

                Arguments.arguments(4, 2, Value.generate("04", 32))
        );
    }

    @ParameterizedTest
    @MethodSource("data")
    public final void testSplitRecoverFixed(final int countGenerated, final int countNeeded, final String inputHex) {
        // original bytes
        final byte[] bytesOriginal = HexCodec.decode(inputHex);
        logger.finest(String.format("ORIGINAL [%s]", inputHex));
        final int sizeData = bytesOriginal.length;
        // split original bytes
        final byte[][] bytesSplit = new byte[countGenerated][sizeData];
        final Random random = new Random(0L);
        for (int i = 0; (i < sizeData); ++i) {
            final byte[] generate = GF256.generate(random, countNeeded - 1, bytesOriginal[i]);
            for (int j = 0; (j < countGenerated); ++j) {
                bytesSplit[j][i] = GF256.evaluate(generate, (byte) (j + 1));
            }
        }
        for (byte[] bytes : bytesSplit) {
            logger.finest(String.format("SPLIT    [%s]", HexCodec.encode(bytes)));
        }
        // recover original bytes
        final byte[] bytesRecover = new byte[sizeData];
        for (int i = 0; (i < sizeData); ++i) {
            final byte[][] parts = new byte[countNeeded][2];
            for (int j = 0; (j < countNeeded); ++j) {
                parts[j][0] = (byte) (j + 1);
                parts[j][1] = bytesSplit[j][i];
            }
            bytesRecover[i] = GF256.recover(parts);
        }
        // verify recovery
        logger.finest(String.format("RECOVER  [%s]", HexCodec.encode(bytesRecover)));
        Assertions.assertArrayEquals(bytesOriginal, bytesRecover);
    }
}
