package io.github.greyp9.arwo.core.resource.test;

import io.github.greyp9.arwo.core.resource.Pather;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PatherTest {

    @Test
    void name() {
        Assertions.assertEquals("/a/b/", Pather.getParent("/a/b/c"));
        Assertions.assertEquals("/a/", Pather.getParent("/a/b/"));
        Assertions.assertEquals("/a/", Pather.getParent("/a/b"));
    }
}
