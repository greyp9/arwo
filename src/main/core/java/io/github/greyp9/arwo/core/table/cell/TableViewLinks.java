package io.github.greyp9.arwo.core.table.cell;

import io.github.greyp9.arwo.core.lang.CompareU;

import java.util.ArrayList;
import java.util.List;

public class TableViewLinks implements Comparable<TableViewLinks> {
    private final List<TableViewLink> links;

    public final List<TableViewLink> getLinks() {
        return links;
    }

    public TableViewLinks(final List<TableViewLink> links) {
        this.links = (links == null) ? new ArrayList<>() : links;
    }

    @Override
    public final int compareTo(final TableViewLinks tableViewLinks) {
        final List<TableViewLink> linksR = tableViewLinks.getLinks();
        final TableViewLink linkLeft = links.isEmpty() ? null : links.get(0);
        final TableViewLink linkRight = linksR.isEmpty() ? null : linksR.get(0);
        return CompareU.compare(linkLeft, linkRight);
    }

    // findbugs
    @Override
    public final boolean equals(final Object o) {
        return ((o instanceof TableViewLinks) && (compareTo((TableViewLinks) o) == 0));
    }

    // findbugs
    @Override
    public final int hashCode() {
        return links.hashCode();
    }
}
