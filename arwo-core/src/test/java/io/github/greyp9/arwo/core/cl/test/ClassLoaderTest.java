package io.github.greyp9.arwo.core.cl.test;

import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.value.Value;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.Driver;
import java.util.logging.Logger;

public class ClassLoaderTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testClassLoaderResource() throws IOException {
        final URL urlUserDir = new File(SystemU.userDir()).toURI().toURL();
        final URL urlUserHome = new File(SystemU.userHome()).toURI().toURL();
        final URL[] urls = new URL[] { urlUserDir, urlUserHome };
        try (URLClassLoader classLoader = new URLClassLoader(urls, getClass().getClassLoader())) {
            final URL resource = classLoader.findResource(".m2");
            Assumptions.assumeTrue(resource != null);
            logger.info(resource.toExternalForm());
            Assertions.assertTrue(resource.toExternalForm().startsWith(urlUserHome.toExternalForm()));
        }
    }

    @Test
    void testClassLoaderClass()
            throws IOException, InstantiationException, IllegalAccessException, ClassNotFoundException {
        final int majorVersion = 42;
        final int minorVersion = 6;
        final File repository = new File(SystemU.userHome(), ".m2/repository");
        final File fileDriver = new File(repository, "org/postgresql/postgresql/42.6.2/postgresql-42.6.2.jar");
        Assumptions.assumeTrue(fileDriver.exists());
        final URL urlDriver = fileDriver.toURI().toURL();
        final URL[] urls = new URL[] { urlDriver };
        try (URLClassLoader classLoader = new URLClassLoader(urls, getClass().getClassLoader())) {
            final Class<?> c = Class.forName("org.postgresql.Driver", true, classLoader);
            final Driver driver = (Driver) c.newInstance();
            Assertions.assertEquals(majorVersion, driver.getMajorVersion());
            Assertions.assertEquals(minorVersion, driver.getMinorVersion());
            final ClassLoader classLoader1 = driver.getClass().getClassLoader();
            final URLClassLoader classLoader2 = Value.as(classLoader1, URLClassLoader.class);
            final URL[] urls2 = classLoader2.getURLs();
            Assertions.assertEquals(1, urls2.length);
            Assertions.assertEquals(urlDriver, urls2[0]);
        }
    }
}
