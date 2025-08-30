package io.github.greyp9.arwo.core.config.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import java.io.IOException;

public class PreferencesTest {

    @Test
    void testPreferencesGetMimeType() throws IOException {
        final XsdTypes xsdTypes = new XsdTypes(ResourceU.resolve(App.Config.XSD), null, null);
        final Document document = DocumentU.toDocument(StreamU.read(
                ResourceU.resolve("io/github/greyp9/arwo/app/app-test.xml")));
        final Xed xed = new Xed(document, xsdTypes);

        final Preferences preferences = new Preferences(xed);
        // old API
        Assertions.assertEquals("application/gzip", preferences.getMIMEType("foo.tar.gz"));
        Assertions.assertEquals("application/tar+gzip", preferences.getMIMEType("foo.tgz"));
        // new API
        Assertions.assertEquals("application/tar+gzip", preferences.getMIMETypeIt("foo.tar.gz"));
        Assertions.assertEquals("application/gzip", preferences.getMIMETypeIt("foo.bar.gz"));
    }
}
