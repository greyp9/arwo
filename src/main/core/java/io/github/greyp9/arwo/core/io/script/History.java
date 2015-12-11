package io.github.greyp9.arwo.core.io.script;

import io.github.greyp9.arwo.core.date.DateX;

import java.util.ArrayList;
import java.util.Collection;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class History {
    private final Collection<Script> scripts;

    public History() {
        this.scripts = new ArrayList<Script>();
    }

    public final synchronized Collection<Script> getHistory() {
        return scripts;
    }

    public final synchronized void add(final Script script) {
        scripts.add(script);
    }

    public final synchronized Script find(final String idURL) {
        Script scriptFound = null;
        final DateX dateX = DateX.Factory.createURL();
        for (final Script scriptIt : scripts) {
            final String idIt = dateX.toString(scriptIt.getDate());
            if (idIt.equals(idURL)) {
                scriptFound = scriptIt;
            }
        }
        return scriptFound;
    }
}
