package io.github.greyp9.arwo.core.value.test;

import io.github.greyp9.arwo.core.value.Value;
import junit.framework.TestCase;
import org.junit.Assert;

import java.util.ArrayList;
import java.util.List;

public class ValueTest extends TestCase {

    public void testJoin() throws Exception {
        List<String> tokens = new ArrayList<String>();
        tokens.add("a");
        tokens.add("b");
        tokens.add("c");
        Assert.assertEquals("a b c", Value.joinCollection(" ", tokens));
    }
}
