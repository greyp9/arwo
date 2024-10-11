package io.github.greyp9.arwo.core.env;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.ff.DataSplitter;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.jce.KeyCodec;
import io.github.greyp9.arwo.core.jce.KeyU;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.crypto.SecretKey;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

/**
 * Utility functions which may be used to manage a secret derived from a given process environment.
 */
public final class EnvironmentSecret {
    private static final Logger LOGGER = Logger.getLogger(EnvironmentSecret.class.getName());

    private EnvironmentSecret() {
    }

    public static EnvironmentStore generate(final byte[] secret, final EnvironmentState state, final Random random) {
        final List<EnvironmentAtom> atoms = state.getAtoms();
        final int shareCount = atoms.size();
        final int thresholdCount = state.getThreshold();
        final DataSplitter dataSplitter = new DataSplitter(shareCount, thresholdCount, random);
        final byte[][] sharesBytes = dataSplitter.split(secret);
        final List<EnvironmentShare> shares = new ArrayList<>();
        for (int i = 0; (i < shareCount); ++i) {
            shares.add(new EnvironmentShare(atoms.get(i), sharesBytes[i]));
        }
        return new EnvironmentStore(shares);
    }

    public static byte[] recover(final EnvironmentState state, final EnvironmentStore store) {
        final int shareCount = store.getShares().size();
        final int thresholdCount = state.getThreshold();
        final byte[][] shares = new byte[shareCount][];
        for (int i = 0; (i < shares.length); ++i) {
            shares[i] = store.getShares().get(i).getShare();
        }
        final DataSplitter dataSplitter = new DataSplitter(shareCount, thresholdCount, null);
        return dataSplitter.join(shares);
    }

    public static EnvironmentStore protect(
            final EnvironmentStore store, final Random random) throws GeneralSecurityException {
        final List<EnvironmentShare> sharesProtected = new ArrayList<>();
        for (EnvironmentShare environmentShare : store.getShares()) {
            sharesProtected.add(protect(environmentShare, random));
        }
        return new EnvironmentStore(sharesProtected);
    }

    private static EnvironmentShare protect(
            final EnvironmentShare share, final Random random) throws GeneralSecurityException {
        final EnvironmentAtom atom = share.getAtom();
        final byte[] shareBytes = share.getShare();
        final String password = atom.getValue();
        final byte[] salt = HashU.sha256(UTF8Codec.toBytes(atom.getKey()));
        final SecretKey key = KeyU.toKeyPBE(password.toCharArray(), salt);
        final KeyCodec keyCodec = new KeyCodec(key, KeyX.Const.TRANSFORM_GCM, KeyX.Const.PARAM_SPEC_GCM, random);
        return new EnvironmentShare(atom, keyCodec.encode(shareBytes));
    }

    public static EnvironmentStore unprotect(final EnvironmentStore store) throws GeneralSecurityException {
        final List<EnvironmentShare> shares = new ArrayList<>();
        for (EnvironmentShare share : store.getShares()) {
            unprotect(shares, share);
        }
        return new EnvironmentStore(shares);
    }

    private static void unprotect(final List<EnvironmentShare> shares,
                                  final EnvironmentShare share) throws GeneralSecurityException {
        final EnvironmentAtom atom = share.getAtom();
        if (atom != null) {
            final byte[] shareBytes = share.getShare();
            final String password = atom.getValue();
            final byte[] salt = HashU.sha256(UTF8Codec.toBytes(atom.getKey()));
            final SecretKey key = KeyU.toKeyPBE(password.toCharArray(), salt);
            final KeyCodec keyCodec = new KeyCodec(key, KeyX.Const.TRANSFORM_GCM, KeyX.Const.PARAM_SPEC_GCM, null);
            try {
                final byte[] bytesRecover = keyCodec.decode(shareBytes);
                shares.add(new EnvironmentShare(atom, bytesRecover));
            } catch (GeneralSecurityException e) {
                LOGGER.fine(() -> String.format("FAILED TO RECOVER ORDINAL %d", atom.getOrdinal()));
            }
        }
    }

    public static byte[] serialize(final EnvironmentStore store) throws IOException {
        final Document document = DocumentU.createDocument(Const.SHARES, Const.URI);
        final Element element = document.getDocumentElement();
        for (EnvironmentShare share : store.getShares()) {
            final EnvironmentAtom atom = share.getAtom();
            ElementU.addElement(element, Const.ATOM, Base64Codec.encode(share.getShare()), new NameTypeValues(
                    new NameTypeValue(Const.ORDINAL, Integer.toString(atom.getOrdinal()))));
        }
        return DocumentU.toXml(document);
    }

    public static EnvironmentStore deserialize(final byte[] xml, final EnvironmentState state) throws IOException {
        final List<EnvironmentShare> shares = new ArrayList<>();
        final Document document = DocumentU.toDocument(xml);
        for (Element element : ElementU.getChildren(document.getDocumentElement(), Const.ATOM, Const.URI)) {
            final int ordinal = Integer.parseInt(ElementU.getAttribute(element, Const.ORDINAL));
            final EnvironmentAtom atom = state.getAtoms().stream()
                    .filter(a -> a.getOrdinal() == ordinal)
                    .findFirst().orElse(null);
            final byte[] share = Base64Codec.decode(ElementU.getTextContent(element));
            shares.add(new EnvironmentShare(atom, share));
        }
        return new EnvironmentStore(shares);
    }

    public static class Const {
        private static final String URI = "urn:arwo:env";
        private static final String ATOM = "atom";
        private static final String ORDINAL = "ordinal";
        private static final String SHARES = "shares";
    }
}
