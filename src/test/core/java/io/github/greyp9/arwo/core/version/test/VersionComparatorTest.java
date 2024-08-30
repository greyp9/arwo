package io.github.greyp9.arwo.core.version.test;

import io.github.greyp9.arwo.core.version.VersionComparator;
import io.github.greyp9.arwo.core.version.VersionU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.IntPredicate;
import java.util.stream.Stream;

class VersionComparatorTest {

    //private static final String PATTERN_VERSION_ALT = "\\W+";  // includes "_"
    private static final String PATTERN_VERSION_ALNUM = "[^a-zA-Z0-9]+";

    private static Stream<Arguments> data1() {
        return Stream.of(
                Arguments.arguments("", Collections.singletonList("")),
                Arguments.arguments("1", Collections.singletonList("1")),
                Arguments.arguments("11", Collections.singletonList("11")),
                Arguments.arguments("1.2", Arrays.asList("1", "2")),
                Arguments.arguments("1.2.", Arrays.asList("1", "2")),
                Arguments.arguments("1.2.3", Arrays.asList("1", "2", "3")),
                Arguments.arguments("1.2.a", Arrays.asList("1", "2", "a")),
                Arguments.arguments("1.2-3", Arrays.asList("1", "2", "3")),
                Arguments.arguments("1.2-a", Arrays.asList("1", "2", "a"))
        );
    }

    @ParameterizedTest
    @MethodSource("data1")
    void testTokenizeVersion(final String input, final List<String> tokensExpected) {
        final String[] split = input.split(PATTERN_VERSION_ALNUM);
        Assertions.assertEquals(tokensExpected.size(), split.length);
        Assertions.assertEquals(tokensExpected, Arrays.asList(split));
    }

    private static Stream<Arguments> data2() {
        final IntPredicate lt = (f -> f < 0);
        final IntPredicate gt = (f -> f > 0);
        final IntPredicate eq = (f -> f == 0);
        return Stream.of(
                Arguments.arguments("1", "1", eq),
                Arguments.arguments("1", "2", lt),
                Arguments.arguments("2", "1", gt),
                Arguments.arguments("9", "11", lt),
                Arguments.arguments("1.2", "1.11", lt),
                Arguments.arguments("1.2", "1.a", lt),
                Arguments.arguments("1.a", "1.b", lt)
        );
    }

    @ParameterizedTest
    @MethodSource("data2")
    void testVersionCompare(final String left, final String right, final IntPredicate fn) {
        Assertions.assertTrue(fn.test(VersionU.compare(left, right)));
    }

    @Test
    void testComparator() {
        final List<String> versions = new ArrayList<>(Arrays.asList(
                "11", "a", "1", "A", "9"));
        versions.sort(new VersionComparator());
        Assertions.assertEquals("1", versions.remove(0));
        Assertions.assertEquals("9", versions.remove(0));
        Assertions.assertEquals("11", versions.remove(0));
        Assertions.assertEquals("A", versions.remove(0));
        Assertions.assertEquals("a", versions.remove(0));
        Assertions.assertTrue(versions.isEmpty());
    }
}
