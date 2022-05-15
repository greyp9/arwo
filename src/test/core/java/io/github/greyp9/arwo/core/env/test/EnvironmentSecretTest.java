package io.github.greyp9.arwo.core.env.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.env.EnvironmentSecret;
import io.github.greyp9.arwo.core.env.EnvironmentShare;
import io.github.greyp9.arwo.core.env.EnvironmentState;
import io.github.greyp9.arwo.core.env.EnvironmentStore;
import io.github.greyp9.arwo.core.jce.AES;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Random;
import java.util.logging.Logger;

public class EnvironmentSecretTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testSplitRecover() throws GeneralSecurityException {
        final EnvironmentState state = collectState();
        final byte[] secret = AES.generate().getEncoded();
        final EnvironmentStore store = EnvironmentSecret.generate(secret, state, new Random(0L));
        final byte[] secretRecover = EnvironmentSecret.recover(state, store);
        Assert.assertArrayEquals("recovered secret should match original", secret, secretRecover);
    }

    @Test
    public void testSplitProtectRecover() throws GeneralSecurityException {
        final EnvironmentState state = collectState();
        final byte[] secret = AES.generate().getEncoded();
        final Random random = new Random(0L);
        final EnvironmentStore store = EnvironmentSecret.generate(secret, state, random);
        final EnvironmentStore storeWrapped = EnvironmentSecret.protect(store, random);
        final EnvironmentStore storeUnwrapped = EnvironmentSecret.unprotect(storeWrapped);
        final byte[] secretRecover = EnvironmentSecret.recover(state, storeUnwrapped);
        Assert.assertArrayEquals("recovered secret should match original", secret, secretRecover);
    }

    @Test
    public void testSplitProtectSerializeRecover() throws GeneralSecurityException, IOException {
        final EnvironmentState state = collectState();
        final byte[] secret = AES.generate().getEncoded();
        final Random random = new Random(0L);
        final EnvironmentStore store = EnvironmentSecret.generate(secret, state, random);
        final EnvironmentStore storeWrapped = EnvironmentSecret.protect(store, random);
        final byte[] shareXml = EnvironmentSecret.serialize(storeWrapped);
        logger.finest(() -> UTF8Codec.toString(shareXml));
        final EnvironmentStore storeWrappedA = EnvironmentSecret.deserialize(shareXml, state);
        final EnvironmentStore storeUnwrapped = EnvironmentSecret.unprotect(storeWrappedA);
        final byte[] secretRecover = EnvironmentSecret.recover(state, storeUnwrapped);
        Assert.assertArrayEquals("recovered secret should match original", secret, secretRecover);
    }

    @Test
    public void testAtThreshold() throws GeneralSecurityException, IOException {
        final EnvironmentState state = collectState();
        final byte[] secret = AES.generate().getEncoded();
        final Random random = new Random(0L);
        final EnvironmentStore store = EnvironmentSecret.generate(secret, state, random);
        final EnvironmentStore storeWrapped = EnvironmentSecret.protect(store, random);
        final byte[] shareXml = EnvironmentSecret.serialize(storeWrapped);
        logger.finest(() -> UTF8Codec.toString(shareXml));
        final EnvironmentState stateOneOff = collectStateOneOff();
        final EnvironmentStore storeWrappedA = EnvironmentSecret.deserialize(shareXml, stateOneOff);
        final EnvironmentStore storeUnwrapped = EnvironmentSecret.unprotect(storeWrappedA);
        final byte[] secretRecover = EnvironmentSecret.recover(stateOneOff, storeUnwrapped);
        Assert.assertArrayEquals("recovered secret should match original", secret, secretRecover);
    }

    @Test
    public void testBelowThreshold() throws GeneralSecurityException, IOException {
        final EnvironmentState state = collectState();
        final byte[] secret = AES.generate().getEncoded();
        logger.finest(() -> HexCodec.encode(secret));
        final Random random = new Random(0L);
        final EnvironmentStore store = EnvironmentSecret.generate(secret, state, random);
        store.getShares().stream().map(EnvironmentShare::getShare).map(HexCodec::encode).forEach(logger::finest);
        final EnvironmentStore storeWrapped = EnvironmentSecret.protect(store, random);
        final byte[] shareXml = EnvironmentSecret.serialize(storeWrapped);
        logger.finest(() -> UTF8Codec.toString(shareXml));
        final EnvironmentState stateTwoOff = collectStateTwoOff();
        final EnvironmentStore storeWrappedA = EnvironmentSecret.deserialize(shareXml, stateTwoOff);
        final EnvironmentStore storeUnwrapped = EnvironmentSecret.unprotect(storeWrappedA);
        final byte[] secretRecover = EnvironmentSecret.recover(stateTwoOff, storeUnwrapped);
        logger.finest(() -> HexCodec.encode(secretRecover));
        Assert.assertNotEquals("secret should be unrecoverable when threshold not met",
                HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    private EnvironmentState collectState() {
        final EnvironmentAtom atom1 = new EnvironmentAtom(0, "env('HOME')", "~");
        final EnvironmentAtom atom2 = new EnvironmentAtom(1, "prop('file.encoding')", "UTF-8");
        final EnvironmentAtom atom3 = new EnvironmentAtom(2, "mod('~')", "1000000");
        return new EnvironmentState("myenv", null, 2, atom1, atom2, atom3);
    }

    private EnvironmentState collectStateOneOff() {
        final EnvironmentAtom atom1 = new EnvironmentAtom(0, "env('HOME')", "~");
        final EnvironmentAtom atom2 = new EnvironmentAtom(1, "prop('file.encoding')", "UTF-8");
        final EnvironmentAtom atom3 = new EnvironmentAtom(2, "mod('~')", "2000000");
        return new EnvironmentState("myenv", null, 2, atom1, atom2, atom3);
    }

    private EnvironmentState collectStateTwoOff() {
        final EnvironmentAtom atom1 = new EnvironmentAtom(0, "env('HOME')", "~");
        final EnvironmentAtom atom2 = new EnvironmentAtom(1, "prop('file.encoding')", "UTF-16");
        final EnvironmentAtom atom3 = new EnvironmentAtom(2, "mod('~')", "2000000");
        return new EnvironmentState("myenv", null, 2, atom1, atom2, atom3);
    }
}
