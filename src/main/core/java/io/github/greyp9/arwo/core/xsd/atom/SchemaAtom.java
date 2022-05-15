package io.github.greyp9.arwo.core.xsd.atom;

import javax.xml.namespace.QName;
import java.net.URL;

public class SchemaAtom {
    private final URL url;
    private final QName name;
    private final XsdAtom atom;

    public SchemaAtom(final URL url, final QName name, final XsdAtom atom) {
        this.url = url;
        this.name = name;
        this.atom = atom;
    }

    public final URL getURL() {
        return url;
    }

    public final QName getQName() {
        return name;
    }

    public final XsdAtom getAtom() {
        return atom;
    }

    @Override
    public final String toString() {
        return name.toString();
    }
}
