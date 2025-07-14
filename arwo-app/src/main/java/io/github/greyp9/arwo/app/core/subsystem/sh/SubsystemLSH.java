package io.github.greyp9.arwo.app.core.subsystem.sh;

import io.github.greyp9.arwo.app.local.sh2.core.CommandLSH;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.exec.script.ScriptContext;
import io.github.greyp9.arwo.core.exec.script.ScriptSerializer;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.time.Stopwatch;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class SubsystemLSH {
    private final List<CommandLSH> commands;
    private final List<ScriptContext> scripts;
    private final File folderPersist;

    public final List<CommandLSH> getCommands() {
        return commands;
    }

    public final List<ScriptContext> getScripts() {
        return scripts;
    }

    public final File getFolderPersist() {
        return folderPersist;
    }

    public SubsystemLSH(final Alerts alerts, final File folderUserRoot) {
        this.commands = new ArrayList<>();
        this.scripts = new ArrayList<>();
        this.folderPersist = FileU.ensureFolder(new File(folderUserRoot, "result/lsh2"));
        loadRecent(alerts);
    }

    private void loadRecent(final Alerts alerts) {
        final Stopwatch stopwatch = new Stopwatch(getClass().getSimpleName());
        final ScriptSerializer scriptSerializer = new ScriptSerializer();
        final Collection<File> files = new FindInFolderQuery(folderPersist, "*.xml", false).getFound();
        final long skip = Math.max(0, (files.size() - COUNT_RECENT));
        final List<File> filesRecent = files.stream().sorted().skip(skip).collect(Collectors.toList());
        for (final File file : filesRecent) {
            try {
                scripts.add(scriptSerializer.read(file));
            } catch (IOException e) {
                alerts.add(new Alert(Alert.Severity.ERR, e.getMessage()));
            }
        }
        Logger.getLogger(getClass().getName()).info(String.format("init():%d", stopwatch.elapsed()));
    }

    private static final int COUNT_RECENT = 50;
}
