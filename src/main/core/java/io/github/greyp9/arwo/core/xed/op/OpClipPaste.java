package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.clip.XedFragment;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;

public class OpClipPaste {
    private final XedClipboard clipboard;

    public OpClipPaste(final XedClipboard clipboard) {
        this.clipboard = clipboard;
    }

    public final void paste(final XedCursor cursor) throws IOException {
        if (XedClipboard.U.canPaste(cursor)) {
            final Collection<XedFragment> fragments = clipboard.getFragments();
            for (final XedFragment fragment : fragments) {
                if (fragment.getTypeInstance().equals(cursor.getTypeInstance())) {
                    pasteFragment(fragment, cursor);
                }
            }
        }
    }

    private void pasteFragment(final XedFragment fragment, final XedCursor cursor) throws IOException {
        final Document documentClone = DocumentU.toDocument(fragment.getXml());
        final Element elementClone = documentClone.getDocumentElement();
        // transfer ownership of new element node to existing document
        final Document document = cursor.getXed().getDocument();
        document.adoptNode(elementClone);
        // insert location
        final XedCursor cursorConcrete = cursor.getParentConcrete();
        final Element elementCursor = cursor.getElement();
        ElementU.addElement(cursorConcrete.getElement(), elementClone, elementCursor);
    }
}
