package io.github.greyp9.arwo.core.envsec.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.envsec.EnvironmentSecret;
import io.github.greyp9.arwo.core.expr.Grammar;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.Tree;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Random;
import java.util.logging.Logger;

@TestMethodOrder(MethodOrderer.MethodName.class)
public class EnvironmentSecretPersistTest {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final File folderTest = new File(SystemU.tempDir(), getClass().getSimpleName());

    private static final Properties PROPERTIES = new Properties();

    @Test
    public void test_0_SetUpState() throws IOException {
        folderTest.mkdir();
        logger.finest(folderTest.getAbsolutePath());
        final File fileExpression = new File(folderTest, "env.txt");
        final String expression = "secret(2 \n"
                + "secret(2 prop('file.encoding') prop('java.runtime.version'))\n"
                + "secret(2 prop('os.name') prop('os.version'))\n"
                + ")";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));
    }

    @Test
    public void test_1_ParseExpression() throws IOException {
        final File fileExpression = new File(folderTest, "env.txt");
        final String expression = UTF8Codec.toString(StreamU.read(fileExpression));
        final Grammar grammar = new Grammar(expression);
        final Tree tree = new Tree(grammar.toNode());
        final MultiOperator root = Optional.of(tree.getRoot())
                .map(MultiOperator.class::cast)
                .orElseThrow(IllegalStateException::new);
        final List<Node> operands = new ArrayList<>(root.getOperands());
        final Node operandThresholdN = operands.remove(0);
        Assertions.assertTrue(operandThresholdN instanceof Operand);
        for (Node child1 : operands) {
            final MultiOperator multi1 = Optional.of(child1)
                    .map(MultiOperator.class::cast)
                    .orElseThrow(IllegalStateException::new);
            Assertions.assertEquals("secret", multi1.getOp());
            final List<Node> children2 = new ArrayList<>(multi1.getOperands());
            final Node operandThresholdNChild = children2.remove(0);
            Assertions.assertTrue(operandThresholdNChild instanceof Operand);
            for (Node child2 : children2) {
                final MultiOperator multi2 = Optional.of(child2)
                        .map(MultiOperator.class::cast)
                        .orElseThrow(IllegalStateException::new);
                Assertions.assertEquals("prop", multi2.getOp());
            }
        }
    }

    @Test
    public void test_2_GenerateSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env.txt");
        final byte[] secret = AES.generate().getEncoded();
        Assertions.assertEquals(AES.Const.KEY_BYTES, secret.length);
        logger.finest("GENERATE:" + HexCodec.encode(secret));
        PROPERTIES.setProperty("secret", HexCodec.encode(secret));
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
    }

    @Test
    public void test_3_RecoverSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env.txt");
        final EnvironmentSecret environmentSecret = new EnvironmentSecret(fileExpression.getPath(), null);
        final byte[] secretRecover = environmentSecret.recover();
        Assertions.assertEquals(PROPERTIES.getProperty("secret"), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_9_TearDownState() {
        // clean up test folder content; then test folder
        new FindInFolderQuery(folderTest, "env.*", false).getFound().forEach(File::delete);
        folderTest.delete();
        Assertions.assertFalse(folderTest.exists());
    }
}
