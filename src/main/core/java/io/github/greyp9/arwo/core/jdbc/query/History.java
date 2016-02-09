package io.github.greyp9.arwo.core.jdbc.query;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.http.Http;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class History {
    private final Collection<Query> queries;
    private final DateX dateX;

    public History() {
        this.queries = new ArrayList<Query>();
        this.dateX = DateX.Factory.createURL();
    }

    public final synchronized Collection<Query> getHistory() {
        return queries;
    }

    public final synchronized void add(final Query query) {
        queries.add(query);
    }

    public final synchronized Query find(final String id) {
        Query queryFound = null;
        for (final Query queryIt : queries) {
            final String idIt = queryIt.getID();
            if (idIt.equals(id)) {
                queryFound = queryIt;
            }
        }
        return queryFound;
    }

    public final synchronized String getNewID(final Date date) {
        final String idBase = dateX.toString(date);
        int i = 0;
        String id = idBase;
        while (find(id) != null) {
            id = (idBase + Http.Token.HYPHEN + (++i));
        }
        return id;
    }
}
