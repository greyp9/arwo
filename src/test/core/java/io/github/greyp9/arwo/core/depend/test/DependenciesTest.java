package io.github.greyp9.arwo.core.depend.test;

import io.github.greyp9.arwo.core.depend.Dependency;
import io.github.greyp9.arwo.core.depend.DependencyResolver;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.TreeSet;

public class DependenciesTest {
    //private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    @Disabled
    public void testResolveDependencyGanymed() throws Exception {
        final DependencyResolver resolver = new DependencyResolver(
                "io/github/greyp9/irby/core/depend/dependencies.xml");
        // sftp
        final Collection<Dependency> dependenciesSFTP = resolver.resolveDependencies(
                "io.github.greyp9.arwo.app.ssh.servlet.SFTPServlet");
        Assertions.assertEquals(1, dependenciesSFTP.size());
        final Dependency dependencySFTP = dependenciesSFTP.iterator().next();
        Assertions.assertTrue(dependencySFTP.getSrc().contains("ganymed-ssh2-262.jar"));
        // sh
        final Collection<Dependency> dependenciesSH = resolver.resolveDependencies(
                "io.github.greyp9.arwo.app.ssh.servlet.SHServlet");
        Assertions.assertEquals(1, dependenciesSH.size());
        final Dependency dependencySH = dependenciesSH.iterator().next();
        Assertions.assertTrue(dependencySH.getSrc().contains("ganymed-ssh2-262.jar"));
        // conglomerate
        final Collection<Dependency> dependencies = new TreeSet<Dependency>();
        dependencies.addAll(dependenciesSFTP);
        dependencies.addAll(dependenciesSH);
        Assertions.assertEquals(1, dependencies.size());
    }
}
