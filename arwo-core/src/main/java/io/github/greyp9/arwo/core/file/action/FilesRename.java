package io.github.greyp9.arwo.core.file.action;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

public final class FilesRename {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final File folder;
    private final String source;
    private final String target;
    private final Alerts alerts;

    public FilesRename(final File folder, final String source, final String target, final Alerts alerts) {
        this.folder = folder;
        this.source = source;
        this.target = target;
        this.alerts = alerts;
    }

    public void run() {
        execute(new File(folder, source), new File(folder, target));
    }

    private void execute(final File filePatternSource, final File filePatternTarget) {
        final File folderSource = filePatternSource.getParentFile();
        final File folderTarget = filePatternTarget.getParentFile();
        FileU.ensureFolders(folderTarget);
        final Pattern pattern = Pattern.compile(filePatternSource.getName());
        final FindInFolderQuery query = new FindInFolderQuery(folderSource, pattern, false);
        final Collection<File> files = query.getFound();
        final Level level = files.isEmpty() ? Level.FINEST : Level.FINE;
        logger.log(level, String.format("FOUND=%d", files.size()));
        for (File file : files) {
            execute(file, folderTarget, filePatternTarget.getName());
        }
    }


    private void execute(final File fileSource, final File folderTarget, final String filenamePatternTarget) {
        // target filename derived from source filename
        final String filenameTarget = filenamePatternTarget.replace("*", fileSource.getName());
        // target filename timestamp
        final String date = DateX.toFilename(new Date(fileSource.lastModified()));
        final String dateNN = Value.defaultOnEmpty(date, "");
        final String filenameTarget2 = filenameTarget.replace("$DATE", dateNN);
        final File fileTarget = new File(folderTarget, filenameTarget2);
        if (!fileTarget.exists()) {
            executeToFile(fileSource, fileTarget);
        }
    }

    private void executeToFile(final File fileSource, final File fileTarget) {
        try {
            FileU.move(fileSource, fileTarget);
            alerts.add(new Alert(Alert.Severity.INFO, String.format("RENAMED: SOURCE=[%s] TARGET=[%s]",
                    fileSource.getName(), fileTarget.getName())));
        } catch (IOException e) {
            logger.warning(e.getMessage());
        }
    }
}
