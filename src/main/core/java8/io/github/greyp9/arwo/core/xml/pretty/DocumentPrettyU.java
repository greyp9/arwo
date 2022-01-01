package io.github.greyp9.arwo.core.xml.pretty;

import org.w3c.dom.Document;

import java.io.IOException;

@SuppressWarnings("PMD.TooManyMethods")
public final class DocumentPrettyU {

    private DocumentPrettyU() {
    }

    @SuppressWarnings({"PMD.OnlyOneReturn", "PMD.AvoidCatchingThrowable"})
    public static byte[] toXmlPretty(final Document document) throws IOException {
        try {
            return io.github.greyp9.arwo.core.xml.priv.XmlPriv.toXmlPretty(document);
        } catch (Throwable e) {
            return io.github.greyp9.arwo.core.xml.ls.XmlLS.toXmlPretty(document);
        }
    }
}
