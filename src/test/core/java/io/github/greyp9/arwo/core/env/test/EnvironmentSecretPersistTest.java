package io.github.greyp9.arwo.core.env.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.env.EnvironmentSecret;
import io.github.greyp9.arwo.core.env.EnvironmentState;
import io.github.greyp9.arwo.core.env.EnvironmentStore;
import io.github.greyp9.arwo.core.env.eval.AtomNode;
import io.github.greyp9.arwo.core.env.eval.KeyNode;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.eval.ExpressionEvaluator;
import io.github.greyp9.arwo.core.expr.eval.ExpressionEvaluatorFactory;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.logging.Logger;
import java.util.stream.Collectors;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class EnvironmentSecretPersistTest {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final File folderTest = new File(SystemU.tempDir(), getClass().getSimpleName());
    private static final Properties PROPERTIES = new Properties();

    @Test
    public void test_0_InitState() throws IOException {
        folderTest.mkdir();
        final File fileExpression = new File(folderTest, "env.txt");
        final String expression = "key(3 prop('file.encoding') env('HOME') mod('~/Downloads'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));
    }

    @Test
    public void test_1_GenerateSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env.txt");
        final File fileEnvironment = new File(folderTest, "env.xml");
        final EnvironmentState state = collectEnvironmentState(fileExpression);
        final byte[] secret = AES.generate().getEncoded();
        Assert.assertEquals(AES.Const.KEY_BYTES, secret.length);
        logger.finest("GENERATE:" + HexCodec.encode(secret));
        PROPERTIES.setProperty("secret", HexCodec.encode(secret));
        final Random random = new Random(0L);
        final EnvironmentStore store = EnvironmentSecret.generate(secret, state, random);
        final EnvironmentStore storeWrapped = EnvironmentSecret.protect(store, random);
        final byte[] shareXml = EnvironmentSecret.serialize(storeWrapped);
        StreamU.write(fileEnvironment, shareXml);
    }

    @Test
    public void test_2_RecoverSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env.txt");
        final File fileEnvironment = new File(folderTest, "env.xml");
        final EnvironmentState state = collectEnvironmentState(fileExpression);
        final byte[] shareXml = StreamU.read(fileEnvironment);
        final EnvironmentStore storeWrappedA = EnvironmentSecret.deserialize(shareXml, state);
        final EnvironmentStore storeUnwrapped = EnvironmentSecret.unprotect(storeWrappedA);
        final byte[] secret = EnvironmentSecret.recover(state, storeUnwrapped);
        logger.finest("RECOVER:" + HexCodec.encode(secret));
        Assert.assertEquals(AES.Const.KEY_BYTES, secret.length);
        Assert.assertEquals(PROPERTIES.getProperty("secret"), HexCodec.encode(secret));
    }

    private EnvironmentState collectEnvironmentState(final File fileExpression) throws IOException {
        final String expression = UTF8Codec.toString(StreamU.read(fileExpression));
        final ExpressionEvaluator evaluator = ExpressionEvaluatorFactory.create();
        final Node node = evaluator.evaluate(expression);
        Assert.assertTrue(node instanceof KeyNode);
        final KeyNode keyNode = (KeyNode) node;
        final List<EnvironmentAtom> atoms = keyNode.getAtomNodes().stream()
                .map(AtomNode::getAtom).collect(Collectors.toList());
        return new EnvironmentState(null, null, keyNode.getThreshold(), atoms);
    }

    @Test
    public void test_Z_TearDownEnvSecretState() {
        // clean up test folder content; then test folder
        new FindInFolderQuery(folderTest, "env.*", false).getFound().forEach(File::delete);
        folderTest.delete();
    }
}
