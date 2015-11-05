package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.clip.XedFragment;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xml.DocumentU;
import org.w3c.dom.Element;

import java.io.IOException;

public class OpClipCopy {
    private final XedClipboard clipboard;

    public OpClipCopy(final XedClipboard clipboard) {
        this.clipboard = clipboard;
    }

    public final void copy(final XedCursor cursor) throws IOException {
        final Element element = cursor.getElement();
        if (element != null) {
            final XedFragment fragment = new XedFragment(cursor.getTypeInstance(), DocumentU.toString(element));
            clipboard.getFragments().add(fragment);
        }
    }
}
