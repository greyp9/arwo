package io.github.greyp9.arwo.core.io.script;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.http.Http;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class History {
    private final Collection<Script> scripts;
    private final DateX dateX;

    public History() {
        this.scripts = new ArrayList<>();
        this.dateX = DateX.Factory.createFilenameMilli();
    }

    public final synchronized Collection<Script> getHistory() {
        return scripts;
    }

    public final synchronized void add(final Script script) {
        scripts.add(script);
    }

    public final synchronized void remove(final Script script) {
        scripts.remove(script);
    }

    public final synchronized Script find(final String id) {
        Script scriptFound = null;
        for (final Script scriptIt : scripts) {
            final String idIt = scriptIt.getID();
            if (idIt.equals(id)) {
                scriptFound = scriptIt;
            }
        }
        return scriptFound;
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
