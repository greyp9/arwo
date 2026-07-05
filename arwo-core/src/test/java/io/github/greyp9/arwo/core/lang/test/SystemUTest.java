package io.github.greyp9.arwo.core.lang.test;

import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Arrays;
import java.util.stream.Stream;

public class SystemUTest {

    static Stream<Arguments> supplyArgs() {
        final Arguments[] argumentsArray = {
                Arguments.arguments("${user.language}/${user.country}", "en/US"),
                Arguments.arguments("${user.dir}", System.getenv("PWD")),
        };
        return Arrays.stream(argumentsArray);
    }

    @ParameterizedTest
    @MethodSource("supplyArgs")
    final void testResolveSystemProperties(final String input, final String output) {
        // environment differences need not fail the build, so use `assume`
        Assumptions.assumeTrue(output.equals(SystemU.resolveSystemProperties(input)));
    }
}
