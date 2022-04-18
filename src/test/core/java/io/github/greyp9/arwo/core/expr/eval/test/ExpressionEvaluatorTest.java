package io.github.greyp9.arwo.core.expr.eval.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.env.eval.KeyEvaluator;
import io.github.greyp9.arwo.core.env.eval.KeyNode;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.eval.ExpressionEvaluator;
import io.github.greyp9.arwo.core.expr.eval.LastModifiedEvaluator;
import io.github.greyp9.arwo.core.expr.eval.SysEnvEvaluator;
import io.github.greyp9.arwo.core.expr.eval.SysPropEvaluator;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.logging.Logger;

public class ExpressionEvaluatorTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        logger.finest("setup()");
    }

    @Test
    public void testExpressionAtoms() {
        final ExpressionEvaluator evaluator = new ExpressionEvaluator();
        evaluator.register(PROP, new SysPropEvaluator());
        evaluator.register(ENV, new SysEnvEvaluator());
        evaluator.register(MODIFIED, new LastModifiedEvaluator());

        Assert.assertEquals(UTF8Codec.Const.UTF8, evaluator.evaluateAsString("prop('file.encoding')"));
        Assert.assertEquals(SystemU.userHome(), evaluator.evaluateAsString("env('HOME')"));
        final String lastModified = Long.toString(new File(SystemU.userHome()).lastModified());
        Assert.assertEquals(lastModified, evaluator.evaluateAsString("mod(env('HOME'))"));
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
        Assert.assertTrue(node instanceof KeyNode);
        final KeyNode keyNode = (KeyNode) node;
        Assert.assertEquals(2, keyNode.getThreshold());
        final int expectedAtoms = 3;
        Assert.assertEquals(expectedAtoms, keyNode.getAtomNodes().size());
    }

    private static final String PROP = "prop";
    private static final String ENV = "env";
    private static final String MODIFIED = "mod";
    private static final String KEY = "key";
}
