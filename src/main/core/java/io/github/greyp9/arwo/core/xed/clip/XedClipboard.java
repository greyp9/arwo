package io.github.greyp9.arwo.core.xed.clip;

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
}
