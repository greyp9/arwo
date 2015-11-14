package io.github.greyp9.arwo.core.xed.clip;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.ArrayList;
import java.util.Collection;

public class XedClipboard {
    private final Collection<XedFragment> fragments;

    public final Collection<XedFragment> getFragments() {
        return fragments;
    }

    public XedClipboard() {
        this.fragments = new ArrayList<XedFragment>();
    }

    public static final class U {
        private U() {
        }

        public static boolean canCut(final XedCursor cursor) {
            final Element element = cursor.getElement();
            final Node parentNode = ((element == null) ? null : element.getParentNode());
            final boolean hasNode = (element != null);
            final boolean hasElementParent = (parentNode instanceof Element);
            return (hasNode && hasElementParent);
        }

        public static boolean canCopy(final XedCursor cursor) {
            return canCut(cursor);
        }

        public static boolean canPaste(final XedCursor cursor) {
            final XedCursor cursorParentC = cursor.getParentConcrete();
            return (cursorParentC != null);
        }
    }
}
