package io.github.greyp9.arwo.core.lang.test;

import io.github.greyp9.arwo.core.util.PropertiesU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Properties;

public class PropertiesUTest {

    @Test
    void testPropertyInterpolation() {
        final Properties properties = new Properties();
        properties.setProperty("a", "1");
        properties.setProperty("b", "2");
        properties.setProperty("c", "3");

        Assertions.assertEquals("12", PropertiesU.resolve(properties, "${a}${b}"));
        Assertions.assertEquals("23", PropertiesU.resolve(properties, "${b}${c}"));
    }
}
