package io.github.greyp9.arwo.core.env.test;

import io.github.greyp9.arwo.core.vm.env.Environment;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.logging.Logger;

public class EnvironmentCollectTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testCollectSysProps() {
        final Environment environment = new Environment();
        Assertions.assertNotNull(environment.systemProperty("user.home"));
        Assertions.assertNotNull(environment.systemProperty("java.vm.version"));
        Assertions.assertNotNull(environment.systemProperty("java.io.tmpdir"));
    }

    @Test
    public void testCollectSysEnv() {
        final Environment environment = new Environment();
        Assertions.assertNotNull(environment.environment("PATH"));
        Assertions.assertNotNull(environment.environment("JAVA_HOME"));
        Assertions.assertNotNull(environment.environment("SHELL"));
    }

    @Test
    public void testCollectFile() {
        final File file = new File(System.getProperty("user.dir"));
        logger.finest(file.getPath());
        Assertions.assertTrue(file.exists());
        final Environment environment = new Environment();
        Assertions.assertNotNull(environment.fileLength(file.getPath()));
        Assertions.assertNotNull(environment.fileModified(file.getPath()));
    }
}
