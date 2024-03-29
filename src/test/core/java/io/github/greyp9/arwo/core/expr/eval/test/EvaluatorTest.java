package io.github.greyp9.arwo.core.expr.eval.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.env.eval.AtomNode;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.eval.ExpressionEvaluator;
import io.github.greyp9.arwo.core.expr.eval.FolderEvaluator;
import io.github.greyp9.arwo.core.expr.eval.LastModifiedEvaluator;
import io.github.greyp9.arwo.core.expr.eval.Sha256Evaluator;
import io.github.greyp9.arwo.core.expr.eval.SysEnvEvaluator;
import io.github.greyp9.arwo.core.expr.eval.SysPropEvaluator;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.logging.Logger;

public class EvaluatorTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public final void setUp() throws Exception {
        logger.finest("setup()");
    }

    @Test
    public void testEvaluateExpressionSysProp() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(PROP, new SysPropEvaluator());
        final Node node = evaluator.evaluate("prop('file.encoding')");
        Assertions.assertTrue(node instanceof AtomNode);
        Assertions.assertEquals(UTF8Codec.Const.UTF8, ((AtomNode) node).getAtom().getValue());
    }

    @Test
    public void testEvaluateExpressionSysEnv() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(ENV, new SysEnvEvaluator());
        final Node node = evaluator.evaluate("env('HOME')");
        Assertions.assertTrue(node instanceof AtomNode);
        Assertions.assertEquals(SystemU.userHome(), ((AtomNode) node).getAtom().getValue());
    }

    @Test
    public void testEvaluateExpressionMultiple() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(PROP, new SysPropEvaluator());
        evaluator.register(ENV, new SysEnvEvaluator());
        Assertions.assertEquals(UTF8Codec.Const.UTF8, evaluator.evaluateAsString("prop('file.encoding')"));
        Assertions.assertEquals(SystemU.userHome(), evaluator.evaluateAsString("env('HOME')"));
    }

    @Test
    public void testEvaluateExpressionNotRegistered() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        final IllegalArgumentException exception = Assertions.assertThrows(
                IllegalArgumentException.class, () -> evaluator.evaluate("foo('bar')"));
        Assertions.assertTrue(exception.getMessage().contains("foo"));
    }

    @Test
    public void testEvaluateExpressionFolder() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(ENV, new SysEnvEvaluator());
        evaluator.register(FOLDER, new FolderEvaluator());
        final String content = evaluator.evaluateAsString("folder(env('HOME') '*')");
        Assertions.assertNotNull(content);
        Assertions.assertTrue(content.contains("Downloads"));
        logger.finest(content);
    }

    @Test
    public void testEvaluateExpressionComplex() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(PROP, new SysPropEvaluator());
        evaluator.register(ENV, new SysEnvEvaluator());
        evaluator.register(MODIFIED, new LastModifiedEvaluator());
        evaluator.register(SHA256, new Sha256Evaluator());
        final String lastModified = Long.toString(new File(SystemU.userHome()).lastModified());
        logger.finest(lastModified);
        Assertions.assertEquals(lastModified, evaluator.evaluateAsString("mod(env('HOME'))"));
        Assertions.assertEquals(lastModified, evaluator.evaluateAsString("mod(prop('user.home'))"));
        final String sha256 = HexCodec.encode(HashU.sha256(UTF8Codec.toBytes(lastModified)));
        logger.finest(sha256);
        Assertions.assertEquals(sha256, evaluator.evaluateAsString("sha256(mod(env('HOME')))"));
        Assertions.assertEquals(sha256, evaluator.evaluateAsString("sha256(mod(prop('user.home')))"));
    }

    private static final String PROP = "prop";
    private static final String ENV = "env";
    private static final String MODIFIED = "mod";
    private static final String SHA256 = "sha256";
    private static final String FOLDER = "folder";
}
