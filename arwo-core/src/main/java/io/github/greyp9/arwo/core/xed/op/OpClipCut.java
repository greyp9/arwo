package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.clip.XedFragment;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;

public class OpClipCut {
    private final XedClipboard clipboard;

    public OpClipCut(final XedClipboard clipboard) {
        this.clipboard = clipboard;
    }

    public final void cut(final XedCursor cursor) throws IOException {
        if (XedClipboard.U.canCut(cursor)) {
            final Element element = cursor.getElement();
            final XedFragment fragment = new XedFragment(cursor.getTypeInstance(), DocumentU.toString(element));
            clipboard.getFragments().add(fragment);
            ElementU.detach(element);
        }
    }
}
