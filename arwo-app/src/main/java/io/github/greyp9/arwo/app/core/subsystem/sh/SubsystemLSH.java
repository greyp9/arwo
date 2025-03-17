package io.github.greyp9.arwo.app.core.subsystem.sh;

import io.github.greyp9.arwo.app.local.sh2.core.CommandLSH;

import java.util.ArrayList;
import java.util.List;

public class SubsystemLSH {
    private final List<CommandLSH> commands;

    public final List<CommandLSH> getCommands() {
        return commands;
    }

    public SubsystemLSH() {
        this.commands = new ArrayList<>();
    }
}
