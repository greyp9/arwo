package io.github.greyp9.arwo.core.table.cell;

import io.github.greyp9.arwo.core.lang.CompareU;

public class TableViewGlyph implements Comparable<TableViewGlyph> {
    private final String text;

    public TableViewGlyph(final String text) {
        this.text = text;
    }

    public final String getText() {
        return text;
    }

    @Override
    @SuppressWarnings("NullableProblems")
    public final int compareTo(final TableViewGlyph tableViewGlyph) {
        return CompareU.compare(text, (tableViewGlyph == null) ? null : tableViewGlyph.getText());
    }

    @Override
    public final boolean equals(final Object o) {
        return ((o instanceof TableViewGlyph) && (compareTo((TableViewGlyph) o) == 0));
    }

    @Override
    public final int hashCode() {
        return ((text == null) ? 0 : text.hashCode());
    }
}
