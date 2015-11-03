package io.github.greyp9.arwo.core.table.cell;

import io.github.greyp9.arwo.core.lang.CompareU;

public class TableViewLink implements Comparable<TableViewLink> {
    private final String text;
    private final String title;
    private final String href;

    public final String getText() {
        return text;
    }

    public final String getTitle() {
        return title;
    }

    public final String getHref() {
        return href;
    }

    public TableViewLink(final String text, final String title, final String href) {
        this.text = text;
        this.title = title;
        this.href = href;
    }

    @Override
    public final int compareTo(final TableViewLink tableViewLink) {
        return CompareU.compare(text, (tableViewLink == null) ? null : tableViewLink.getText());
    }

    // findbugs
    @Override
    public final boolean equals(final Object o) {
        return ((o instanceof TableViewLink) && (compareTo((TableViewLink) o) == 0));
    }

    // findbugs
    @Override
    public final int hashCode() {
        return ((text == null) ? 0 : text.hashCode());
    }
}
