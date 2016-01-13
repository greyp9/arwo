package io.github.greyp9.arwo.core.jdbc.query;

import io.github.greyp9.arwo.core.date.DateX;

import java.util.ArrayList;
import java.util.Collection;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class History {
    private final Collection<Query> queries;

    public History() {
        this.queries = new ArrayList<Query>();
    }

    public final synchronized Collection<Query> getHistory() {
        return queries;
    }

    public final synchronized void add(final Query query) {
        queries.add(query);
    }

    public final synchronized Query find(final String idURL) {
        Query queryFound = null;
        final DateX dateX = DateX.Factory.createURL();
        for (final Query queryIt : queries) {
            final String idIt = dateX.toString(queryIt.getDate());
            if (idIt.equals(idURL)) {
                queryFound = queryIt;
            }
        }
        return queryFound;
    }
}
