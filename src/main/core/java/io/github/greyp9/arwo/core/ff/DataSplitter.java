package io.github.greyp9.arwo.core.ff;

import java.util.Random;

public class DataSplitter {

    // https://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing

    private final int shareCount;
    private final int thresholdCount;
    private final Random random;

    public DataSplitter(final int shareCount, final int thresholdCount, final Random random) {
        this.shareCount = shareCount;
        this.thresholdCount = thresholdCount;
        this.random = random;
    }

    /**
     * Split parameter data into shares, based on the DataSplitter config.
     */
    public final byte[][] split(final byte[] data) {
        final int shareLength = data.length + 1;  // shares include an extra byte to preserve share index
        final byte[][] dataShares = new byte[shareCount][shareLength];
        // split each byte of the input data
        for (int i = 0; (i < data.length); ++i) {
            final byte[] generate = GF256.generate(random, thresholdCount - 1, data[i]);
            // populate the share arrays for each byte of the input data
            for (int j = 0; (j < shareCount); ++j) {
                dataShares[j][i] = GF256.evaluate(generate, (byte) j);
            }
        }
        // preserve share indices (needed for data recovery)
        for (int j = 0; (j < shareCount); ++j) {
            dataShares[j][data.length] = (byte) j;
        }
        return dataShares;
    }

    /**
     * Reconstitute the shares into the original data.
     */
    public final byte[] join(final byte[][] dataShares) {
        final int shareCountIn = dataShares.length;
        final int dataLength = dataShares[0].length - 1;  // last byte preserves share index
        final byte[] data = new byte[dataLength];
        // recover each byte of the original input data
        for (int i = 0; (i < dataLength); ++i) {
            final byte[][] parts = new byte[shareCountIn][2];
            for (int j = 0; (j < shareCountIn); ++j) {
                parts[j][0] = dataShares[j][dataLength];  // the share index
                parts[j][1] = dataShares[j][i];
            }
            data[i] = GF256.recover(parts);
        }
        return data;
    }
}
