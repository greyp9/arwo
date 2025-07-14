package io.github.greyp9.arwo.app.core.subsystem.local;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.script.History;
import io.github.greyp9.arwo.core.io.script.read.ScriptReader;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class SubsystemLocal {
    // usage history
    private final History history;
    // persistence
    private final File folderResultLSH;

    public final History getHistory() {
        return history;
    }

    public final File getFolderResultLSH() {
        return folderResultLSH;
    }

    public SubsystemLocal(final Alerts alerts, final File folder) {
        this.history = new History();
        this.folderResultLSH = new File(folder, "result/lsh");
        loadRecent(alerts, folderResultLSH);
    }

    private void loadRecent(final Alerts alerts, final File folderLSH) {
        final Collection<File> files = new FindInFolderQuery(folderLSH, "*.results", false).getFound();
        final long skip = Math.max(0, (files.size() - COUNT_RECENT));
        final List<File> filesRecent = files.stream().sorted().skip(skip).collect(Collectors.toList());
        final ScriptReader scriptReader = new ScriptReader();
        for (final File file : filesRecent) {
            try {
                Optional.ofNullable(scriptReader.readFrom(file.getName(), StreamU.read(file))).ifPresent(history::add);
            } catch (IOException e) {
                alerts.add(new Alert(Alert.Severity.ERR, e.getMessage()));
            }
        }
    }

    private static final int COUNT_RECENT = 50;
}
