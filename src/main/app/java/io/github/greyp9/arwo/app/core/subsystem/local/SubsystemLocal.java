package io.github.greyp9.arwo.app.core.subsystem.local;

import io.github.greyp9.arwo.core.io.script.History;

import java.util.Properties;

public class SubsystemLocal {
    // usage history
    private final History history;
    // properties
    private final Properties properties;

    public final History getHistory() {
        return history;
    }

    public final Properties getProperties() {
        return properties;
    }

    public SubsystemLocal() {
        this.history = new History();
        this.properties = new Properties();
    }
}
