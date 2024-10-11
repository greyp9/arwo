package io.github.greyp9.arwo.core.value.test;

import io.github.greyp9.arwo.core.value.Value;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

public class ValueTest {

    @Test
    public void testJoin() throws Exception {
        List<String> tokens = new ArrayList<String>();
        tokens.add("a");
        tokens.add("b");
        tokens.add("c");
        Assertions.assertEquals("a b c", Value.joinCollection(" ", tokens));
    }

    @Test
    public void testAs() throws Exception {
        final Object anObject = "string";
        final String asString = Value.as(anObject, String.class);
        Assertions.assertNotNull(asString);
        Assertions.assertEquals("string", asString);
        final Integer asInteger = Value.as(anObject, Integer.class);
        Assertions.assertNull(asInteger);
    }
}
