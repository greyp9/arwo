package io.github.greyp9.arwo.core.vm.env;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public final class EnvironmentU {

    private EnvironmentU() {
    }

    public static byte[] getEnv(final Collection<String> skip) throws IOException {
        final List<Pattern> skipPatterns = skip.stream().map(Pattern::compile).collect(Collectors.toList());
        final String uri = App.Config.QNAME_APP.getNamespaceURI();
        final Document document = DocumentU.createDocument("sysenv", uri);
        final Element element = document.getDocumentElement();
        final Map<String, String> environment = System.getenv();
        for (final Map.Entry<String, String> entry : environment.entrySet()) {
            final String key = entry.getKey();
            final String value = entry.getValue();
            final Matcher matcher = skipPatterns.stream().map(p -> p.matcher(key))
                    .filter(Matcher::matches).findAny().orElse(null);
            if (matcher == null) {
                ElementU.addElement(element, "entry", value, NTV.create("key", key));
            }
        }
        return DocumentU.toXml(document);
    }
}
