package io.github.greyp9.arwo.core.lang.test;

import io.github.greyp9.arwo.core.lang.ShellU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ShellTest {

    @Test
    void testString1() {
        final String command = "ls";
        final String[] commandArray = ShellU.toCommandArray(command);
        Assertions.assertEquals(2 + 1, commandArray.length);
        Assertions.assertEquals(command, commandArray[2]);
    }

    @Test
    void testStringN() {
        final String[] command = { "ls", "-l" };
        final String[] commandArray = ShellU.toCommandArray(command);
        Assertions.assertEquals(2 + command.length, commandArray.length);
        for (int index = 0; (index < 2); ++index) {
            Assertions.assertEquals(command[index], commandArray[2 + index]);
        }
    }
}
