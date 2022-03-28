package io.github.greyp9.arwo.core.ff.test;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.ff.DataSplitter;
import io.github.greyp9.arwo.core.value.Value;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

@RunWith(Parameterized.class)
@SuppressWarnings({"checkstyle:magicnumber", "checkstyle:visibilitymodifier"})
public class DataSplitterTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Parameterized.Parameters
    public static Object[][] data() {
        return new Object[][]{
                {2, 2, Value.generate("00", 32), new Random(0L)},
                {3, 3, Value.generate("01", 32), new Random(0L)},
                {2, 2, Value.generate("00", 32), new SecureRandom()},
                {3, 3, Value.generate("01", 32), new SecureRandom()},

                {3, 2, Value.generate("02", 32), new Random(0L)},
                {4, 3, Value.generate("03", 32), new Random(0L)},
                {3, 2, Value.generate("02", 32), new SecureRandom()},
                {4, 3, Value.generate("03", 32), new SecureRandom()},

                {4, 2, Value.generate("04", 32), new Random(0L)},
                {5, 3, Value.generate("05", 32), new Random(0L)},
                {4, 2, Value.generate("04", 32), new SecureRandom()},
                {5, 3, Value.generate("05", 32), new SecureRandom()},

                {5, 2, Value.generate("06", 32), new Random(0L)},
                {6, 3, Value.generate("07", 32), new Random(0L)},
                {5, 2, Value.generate("06", 32), new SecureRandom()},
                {6, 3, Value.generate("07", 32), new SecureRandom()},
        };
    }

    @Parameterized.Parameter()
    public int shareCount;

    @Parameterized.Parameter(1)
    public int thresholdCount;

    @Parameterized.Parameter(2)
    public String dataHex;

    @Parameterized.Parameter(3)
    public Random random;

    /**
     * Test ability to recover data given all shares in the split data.
     */
    @Test
    public void testSplitRecover() {
        logger.finest("^ - " + dataHex);
        final byte[] data = HexCodec.decode(dataHex);
        // split the data
        final DataSplitter splitter = new DataSplitter(shareCount, thresholdCount, random);
        final byte[][] shares = splitter.split(data);
        Arrays.stream(shares).forEachOrdered(s -> logger.finest("% - " + HexCodec.encode(s)));
        Assert.assertEquals(shareCount, shares.length);
        Arrays.stream(shares).forEach(s -> Assert.assertEquals(data.length + 1, s.length));  // +1 byte for share index
        // join the data
        final byte[] dataRecover = splitter.join(shares);
        logger.finest("$ - " + HexCodec.encode(dataRecover));
        Assert.assertArrayEquals(data, dataRecover);
    }

    /**
     * Test ability to recover data given some shares in the split data.
     */
    @Test
    public void testSplitRecoverThreshold() {
        logger.finest("^ - " + dataHex);
        final byte[] data = HexCodec.decode(dataHex);
        // split the data
        final DataSplitter splitter = new DataSplitter(shareCount, thresholdCount, random);
        final byte[][] shares = splitter.split(data);
        Arrays.stream(shares).forEachOrdered(s -> logger.finest("% - " + HexCodec.encode(s)));
        Assert.assertEquals(shareCount, shares.length);
        Arrays.stream(shares).forEach(s -> Assert.assertEquals(data.length + 1, s.length));
        // delete shares at random until threshold shares remain
        final byte[][] sharesThreshold = toThreshold(shares, thresholdCount, random);
        Arrays.stream(sharesThreshold).forEachOrdered(s -> logger.finest("% - " + HexCodec.encode(s)));
        // join the data
        final byte[] dataRecover = splitter.join(sharesThreshold);
        logger.finest("$ - " + HexCodec.encode(dataRecover));
        Assert.assertArrayEquals(data, dataRecover);
    }

    private byte[][] toThreshold(final byte[][] shares, final int threshold, final Random randomIt) {
        logger.finest(String.format("SHARES:%d, THRESHOLD:%d", shares.length, threshold));
        final List<byte[]> listBytes = new ArrayList<>(Arrays.asList(shares));
        while (listBytes.size() > threshold) {
            listBytes.remove(randomIt.nextInt(listBytes.size()));
        }
        Collections.shuffle(listBytes, randomIt);  // ensure that shares are usable regardless of provided order
        final byte[][] thresholdShares = new byte[listBytes.size()][];
        int index = -1;
        for (byte[] bytes : listBytes) {
            thresholdShares[++index] = bytes;
        }
        return thresholdShares;
    }
}
