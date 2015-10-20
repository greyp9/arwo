package io.github.greyp9.arwo.core.xed.cursor;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public class XedCursor {
    private final Xed xed;
    private final XedCursor parent;
    private final Node node;
    private final Integer ordinal;
    private final TypeInstance typeInstance;

    public final Xed getXed() {
        return xed;
    }

    public final XedCursor getParent() {
        return parent;
    }

    public final Node getNode() {
        return node;
    }

    public final Integer getOrdinal() {
        return ordinal;
    }

    public final TypeInstance getTypeInstance() {
        return typeInstance;
    }

    public XedCursor(final Xed xed, final XedCursor parent, final Node node,
                     final Integer ordinal, final TypeInstance typeInstance) {
        this.xed = xed;
        this.parent = parent;
        this.node = node;
        this.ordinal = ordinal;
        this.typeInstance = typeInstance;
    }

    public final Element getElement() {
        return ((node instanceof Element) ? ((Element) node) : null);
    }

    public static String getID(final TypeInstance typeInstance, final Integer ordinal) {
        final byte[] id = UTF8Codec.toBytes(getIdentity(typeInstance, ordinal));
        return CRCU.crc32String(id).substring(Const.STRIP_LEADING);
    }

    private static String getIdentity(final TypeInstance typeInstance, final Integer ordinal) {
        return String.format("%s/%d", typeInstance.toString(), ordinal);
    }

    public final String getURI() {
        return ((parent == null) ? "/" : (parent.getURI() + getID(typeInstance, ordinal) + "/"));
    }

    private static class Const {
        private static final int STRIP_LEADING = 3;
    }
}
