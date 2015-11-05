package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xed.clip.XedClipboard;

import java.io.IOException;

public class OpClipClear {
    private final XedClipboard clipboard;

    public OpClipClear(final XedClipboard clipboard) {
        this.clipboard = clipboard;
    }

    public final void clear() throws IOException {
        clipboard.getFragments().clear();
    }
}
