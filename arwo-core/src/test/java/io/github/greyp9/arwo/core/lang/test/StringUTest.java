package io.github.greyp9.arwo.core.lang.test;

import io.github.greyp9.arwo.core.lang.StringU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.regex.Pattern;

public class StringUTest {

    @Test
    public void testReplaceGroup2() {
        final Pattern pattern = Pattern.compile("(\\w{3}-\\d{3}\\w-\\w{5})(\\d+)(.*)");
        final String input = "abc-120a-abcde12.abc";
        Assertions.assertEquals("abc-120a-abcdexx.abc", StringU.replaceGroup(pattern, input, 2, "xx"));
    }
}
