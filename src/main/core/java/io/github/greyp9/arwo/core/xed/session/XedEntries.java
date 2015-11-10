package io.github.greyp9.arwo.core.xed.session;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class XedEntries {
    private final Collection<XedEntry> entries;

    public XedEntries(final XedEntry... entries) {
        this.entries = new ArrayList<XedEntry>(Arrays.asList(entries));
    }

    public final int size() {
        return entries.size();
    }

    public final XedEntry get(final String contextPath) {
        XedEntry entry = null;
        for (final XedEntry entryIt : entries) {
            if (contextPath.equals(entryIt.getContextPath())) {
                entry = entryIt;
                break;
            }
        }
        return entry;
    }
}
