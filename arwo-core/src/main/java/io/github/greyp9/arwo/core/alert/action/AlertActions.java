package io.github.greyp9.arwo.core.alert.action;

import java.util.Arrays;
import java.util.Collection;

public class AlertActions {
    private final String id;
    private final Collection<String> options;

    public final String getID() {
        return id;
    }

    public final Collection<String> getOptions() {
        return options;
    }

    public AlertActions(final String id, final String... options) {
        this.id = id;
        this.options = Arrays.asList(options);
    }
}
