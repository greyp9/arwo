package io.github.greyp9.arwo.core.env.test;

import io.github.greyp9.arwo.core.vm.env.Environment;
import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.util.logging.Logger;

public class EnvironmentCollectTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testCollectSysProps() {
        final Environment environment = new Environment();
        Assert.assertNotNull(environment.systemProperty("user.home"));
        Assert.assertNotNull(environment.systemProperty("java.vm.version"));
        Assert.assertNotNull(environment.systemProperty("java.io.tmpdir"));
    }

    @Test
    public void testCollectSysEnv() {
        final Environment environment = new Environment();
        Assert.assertNotNull(environment.environment("PATH"));
        Assert.assertNotNull(environment.environment("JAVA_HOME"));
        Assert.assertNotNull(environment.environment("SHELL"));
    }

    @Test
    public void testCollectFile() {
        final File file = new File(System.getProperty("user.dir"));
        logger.finest(file.getPath());
        Assert.assertTrue(file.exists());
        final Environment environment = new Environment();
        Assert.assertNotNull(environment.fileLength(file.getPath()));
        Assert.assertNotNull(environment.fileModified(file.getPath()));
    }
}
