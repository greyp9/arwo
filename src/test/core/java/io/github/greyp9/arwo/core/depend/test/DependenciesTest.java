package io.github.greyp9.arwo.core.depend.test;

import io.github.greyp9.arwo.core.depend.Dependency;
import io.github.greyp9.arwo.core.depend.DependencyResolver;
import org.junit.Assert;
import org.junit.Test;

import java.util.Collection;
import java.util.TreeSet;

public class DependenciesTest {
    //private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testResolveDependencyGanymed() throws Exception {
        final DependencyResolver resolver = new DependencyResolver(
                "io/github/greyp9/irby/core/depend/dependencies.xml");
        // sftp
        final Collection<Dependency> dependenciesSFTP = resolver.resolveDependencies(
                "io.github.greyp9.arwo.app.ssh.servlet.SFTPServlet");
        Assert.assertEquals(1, dependenciesSFTP.size());
        final Dependency dependencySFTP = dependenciesSFTP.iterator().next();
        Assert.assertTrue(dependencySFTP.getSrc().contains("ganymed-ssh2-262.jar"));
        // sh
        final Collection<Dependency> dependenciesSH = resolver.resolveDependencies(
                "io.github.greyp9.arwo.app.ssh.servlet.SHServlet");
        Assert.assertEquals(1, dependenciesSH.size());
        final Dependency dependencySH = dependenciesSH.iterator().next();
        Assert.assertTrue(dependencySH.getSrc().contains("ganymed-ssh2-262.jar"));
        // conglomerate
        final Collection<Dependency> dependencies = new TreeSet<Dependency>();
        dependencies.addAll(dependenciesSFTP);
        dependencies.addAll(dependenciesSH);
        Assert.assertEquals(1, dependencies.size());
    }
}
