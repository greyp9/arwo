package io.github.greyp9.arwo.core.table.cell;

import io.github.greyp9.arwo.core.lang.CompareU;

public class TableViewButton implements Comparable<TableViewButton> {
    private final String text;
    private final String name;
    private final String value;

    public final String getText() {
        return text;
    }

    public final String getName() {
        return name;
    }

    public final String getValue() {
        return value;
    }

    public TableViewButton(final String text, final String name, final String value) {
        this.text = text;
        this.name = name;
        this.value = value;
    }

    @Override
    public final int compareTo(final TableViewButton tableViewButton) {
        return CompareU.compare(text, (tableViewButton == null) ? null : tableViewButton.getText());
    }

    @Override
    public final boolean equals(final Object o) {
        return ((o instanceof TableViewButton) && (compareTo((TableViewButton) o) == 0));
    }

    @Override
    public final int hashCode() {
        return ((text == null) ? 0 : text.hashCode());
    }
}
