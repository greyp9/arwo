package io.github.greyp9.arwo.core.expr.eval.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.env.eval.KeyEvaluator;
import io.github.greyp9.arwo.core.env.eval.KeyNode;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.eval.ExpressionEvaluator;
import io.github.greyp9.arwo.core.expr.eval.FolderEvaluator;
import io.github.greyp9.arwo.core.expr.eval.LastModifiedEvaluator;
import io.github.greyp9.arwo.core.expr.eval.SysEnvEvaluator;
import io.github.greyp9.arwo.core.expr.eval.SysPropEvaluator;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.logging.Logger;

public class ExpressionEvaluatorTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public final void setUp() throws Exception {
        logger.finest("setup()");
    }

    @Test
    public void testExpressionAtoms() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(PROP, new SysPropEvaluator());
        evaluator.register(ENV, new SysEnvEvaluator());
        evaluator.register(MODIFIED, new LastModifiedEvaluator());

        Assertions.assertEquals(UTF8Codec.Const.UTF8, evaluator.evaluateAsString("prop('file.encoding')"));
        Assertions.assertEquals(SystemU.userHome(), evaluator.evaluateAsString("env('HOME')"));
        final String lastModified = Long.toString(new File(SystemU.userHome()).lastModified());
        Assertions.assertEquals(lastModified, evaluator.evaluateAsString("mod(env('HOME'))"));
    }

    @Test
    public void testExpression() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(PROP, new SysPropEvaluator());
        evaluator.register(ENV, new SysEnvEvaluator());
        evaluator.register(MODIFIED, new LastModifiedEvaluator());
        evaluator.register(KEY, new KeyEvaluator());

        final String expression = "key(2 prop('file.encoding') env('HOME') mod('~/Downloads'))";
        final Node node = evaluator.evaluate(expression);
        Assertions.assertTrue(node instanceof KeyNode);
        final KeyNode keyNode = (KeyNode) node;
        Assertions.assertEquals(2, keyNode.getThreshold());
        final int expectedAtoms = 3;
        Assertions.assertEquals(expectedAtoms, keyNode.getAtomNodes().size());
    }

    @Test
    public void testFolder() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(PROP, new SysPropEvaluator());
        evaluator.register(FOLDER, new FolderEvaluator());
        final String contentUserHome = evaluator.evaluateAsString("folder(prop('user.home') '*')");
        logger.finest(contentUserHome);
        Assertions.assertNotNull(contentUserHome);
        Assertions.assertTrue(contentUserHome.contains("Downloads"));
    }

    private static final String PROP = "prop";
    private static final String ENV = "env";
    private static final String MODIFIED = "mod";
    private static final String KEY = "key";
    private static final String FOLDER = "folder";
}
