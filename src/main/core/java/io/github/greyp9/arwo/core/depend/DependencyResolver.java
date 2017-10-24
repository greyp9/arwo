package io.github.greyp9.arwo.core.depend;

import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPathContext;
import io.github.greyp9.arwo.core.xpath.XPathContextFactory;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;

public class DependencyResolver {
    //private final String resource;
    private final XPather xpather;

    public DependencyResolver(final String resource) throws IOException {
        //this.resource = resource;
        final byte[] bytes = StreamU.read(ResourceU.resolve(resource));
        final Document document = DocumentU.toDocument(bytes);
        final XPathContext context = XPathContextFactory.create(document);
        this.xpather = new XPather(document, context);
    }

    public final Collection<Dependency> resolveDependencies(final String className) throws IOException {
        final Collection<Dependency> dependencies = new TreeSet<Dependency>();
        final String xpath = String.format("/irby:dependencies/irby:binding/irby:entry[@className='%s']", className);
        final List<Element> bindings = xpather.getElements(xpath);
        for (final Element binding : bindings) {
            addDependenciesBinding(dependencies, binding);
        }
        return dependencies;
    }

    private void addDependenciesBinding(Collection<Dependency> dependencies, Element entry) throws IOException {
        final String remoteId = ElementU.getAttribute(entry, "remoteId");
        final String xpath = String.format("/irby:dependencies/irby:resource/irby:entry[@remoteId='%s']", remoteId);
        final List<Element> remoteEntries = xpather.getElements(xpath);
        for (Element remoteEntry : remoteEntries) {
            addDependenciesResource(dependencies, remoteEntry);
        }
    }

    private void addDependenciesResource(Collection<Dependency> dependencies, Element entry) throws IOException {
        final String resource = ElementU.getAttribute(entry, "resource");
        final String md5 = ElementU.getAttribute(entry, "md5");
        final String sha1 = ElementU.getAttribute(entry, "sha1");
        dependencies.add(new Dependency(resource, md5, sha1));
    }
}
