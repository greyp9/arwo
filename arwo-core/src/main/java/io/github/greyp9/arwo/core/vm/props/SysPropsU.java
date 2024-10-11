package io.github.greyp9.arwo.core.vm.props;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.Properties;

public final class SysPropsU {

    private SysPropsU() {
    }

    public static byte[] getProps(final Collection<String> skip) throws IOException {
        final String uri = App.Config.QNAME_APP.getNamespaceURI();
        final Document document = DocumentU.createDocument("sysprops", uri);
        final Element element = document.getDocumentElement();
        final Properties properties = System.getProperties();
        for (Map.Entry<Object, Object> entry : properties.entrySet()) {
            final String key = entry.getKey().toString();
            final String value = entry.getValue().toString();
            if (!skip.contains(key)) {
                ElementU.addElement(element, "entry", value, NTV.create("key", key));
            }
        }
        return DocumentU.toXml(document);
    }
}
