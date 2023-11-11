package io.github.greyp9.arwo.core.env.test;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.env.EnvironmentSecret;
import io.github.greyp9.arwo.core.env.EnvironmentState;
import io.github.greyp9.arwo.core.env.EnvironmentStore;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.value.Value;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Random;
import java.util.logging.Logger;
import java.util.stream.Stream;

@SuppressWarnings({"checkstyle:magicnumber", "checkstyle:visibilitymodifier"})
public class EnvironmentSecretPermuteTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public static Stream<Arguments> data() {
        final Collection<Arguments> parameters = new ArrayList<>();
        final int minShares = 2;
        final int maxShares = 5;
        for (int shares = minShares; (shares <= maxShares); ++shares) {
            for (int threshold = minShares; (threshold <= shares); ++threshold) {
                for (int available = threshold - 1; (available <= shares); ++available) {
                    final Random[] randoms = {new Random(0L)};
                    for (Random random : randoms) {
                        parameters.add(Arguments.arguments(shares, threshold, available, random));
                    }
                }
            }
        }
        return parameters.stream();
    }

    @ParameterizedTest
    @MethodSource("data")
    public final void testSplitProtectSerializeRecover(
            final int shareCount, final int thresholdCount, final int available, final Random random)
            throws GeneralSecurityException, IOException {
        logger.finest(() -> String.format("shares=[%d], threshold=[%d], available=[%d], random=[%s]",
                shareCount, thresholdCount, available, random));

        final EnvironmentState stateInitial = collectState(shareCount, thresholdCount);
        final byte[] secret = AES.generate().getEncoded();
        final EnvironmentStore store = EnvironmentSecret.generate(secret, stateInitial, random);
        final EnvironmentStore storeWrapped = EnvironmentSecret.protect(store, random);
        final byte[] shareXml = EnvironmentSecret.serialize(storeWrapped);

        final EnvironmentState stateRecover = collectState(available, thresholdCount);
        final EnvironmentStore storeWrappedA = EnvironmentSecret.deserialize(shareXml, stateRecover);
        final EnvironmentStore storeUnwrapped = EnvironmentSecret.unprotect(storeWrappedA);
        final byte[] secretRecover = EnvironmentSecret.recover(stateRecover, storeUnwrapped);
        if (available >= thresholdCount) {
            Assertions.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
        } else {
            Assertions.assertNotEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
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
