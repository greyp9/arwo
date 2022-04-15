package io.github.greyp9.arwo.core.env.test;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.env.EnvironmentSecret;
import io.github.greyp9.arwo.core.env.EnvironmentState;
import io.github.greyp9.arwo.core.env.EnvironmentStore;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.value.Value;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;
import java.util.logging.Logger;

@RunWith(Parameterized.class)
@SuppressWarnings({"checkstyle:magicnumber", "checkstyle:visibilitymodifier"})
public class EnvironmentSecretPermuteTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Parameterized.Parameters
    public static Object[][] data() {
        final Collection<Object[]> parameters = new ArrayList<>();
        final int minShares = 2;
        final int maxShares = 5;
        for (int shares = minShares; (shares <= maxShares); ++shares) {
            for (int threshold = minShares; (threshold <= shares); ++threshold) {
                for (int available = threshold - 1; (available <= shares); ++available) {
                    final Random[] randoms = {new Random(0L)};
                    for (Random random : randoms) {
                        parameters.add(new Object[] { shares, threshold, available, random });
                    }
                }
            }
        }
        return parameters.toArray(new Object[0][]);
    }

    @Parameterized.Parameter()
    public int shareCount;

    @Parameterized.Parameter(1)
    public int thresholdCount;

    @Parameterized.Parameter(2)
    public int available;

    @Parameterized.Parameter(3)
    public Random random;

    @Test
    public void testSplitProtectSerializeRecover() throws GeneralSecurityException, IOException {
        logger.finest(() -> String.format("shares=[%d], threshold=[%d], available=[%d], random=[%s]",
                shareCount, thresholdCount, available, random));

        final EnvironmentState stateInitial = collectState(shareCount, thresholdCount);
        final byte[] secret = AES.generate().getEncoded();
        final EnvironmentStore store = EnvironmentSecret.generate(secret, stateInitial, random);
        final EnvironmentStore storeWrapped = EnvironmentSecret.protect(store);
        final byte[] shareXml = EnvironmentSecret.serialize(storeWrapped);

        final EnvironmentState stateRecover = collectState(available, thresholdCount);
        final EnvironmentStore storeWrappedA = EnvironmentSecret.deserialize(shareXml, stateRecover);
        final EnvironmentStore storeUnwrapped = EnvironmentSecret.unprotect(storeWrappedA);
        final byte[] secretRecover = EnvironmentSecret.recover(stateRecover, storeUnwrapped);
        if (available >= thresholdCount) {
            Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
        } else {
            Assert.assertNotEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
        }
    }

    private EnvironmentState collectState(final int shares, final int threshold) {
        final EnvironmentAtom[] atoms = new EnvironmentAtom[shares];
        for (int i = 0; (i < shares); ++i) {
            atoms[i] = new EnvironmentAtom(i, Value.generate("x", i + 1), Value.generate("y", i + 1));
        }
        return new EnvironmentState("resource", null, threshold, atoms);
    }
}
