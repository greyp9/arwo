package io.github.greyp9.arwo.core.value.test;

import io.github.greyp9.arwo.core.value.Value;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class ValueTest {

    @Test
    public void testJoin() throws Exception {
        List<String> tokens = new ArrayList<String>();
        tokens.add("a");
        tokens.add("b");
        tokens.add("c");
        Assert.assertEquals("a b c", Value.joinCollection(" ", tokens));
    }

    @Test
    public void testAs() throws Exception {
        final Object anObject = "string";
        final String asString = Value.as(anObject, String.class);
        Assert.assertNotNull(asString);
        Assert.assertEquals("string", asString);
        final Integer asInteger = Value.as(anObject, Integer.class);
        Assert.assertNull(asInteger);
    }
}
