package io.github.greyp9.arwo.core.file.zip.test;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.file.group.FileRegrouper;
import io.github.greyp9.arwo.core.file.zip.ZipAppender;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.logging.Logger;

public class ZipArchiveZTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private void doGrouping(final Date dateEntry, final Collection<File> grouping) throws IOException {
        boolean isEnabledCreateZip = System.getProperties().isEmpty();
        final String comment = XsdDateU.toXSDZMillis(new Date());
        final String filename = String.format("%s.%s.zip",
                getClass().getSimpleName(), DateX.toFilename(dateEntry));
        final File folderSource = new File(SystemU.tempDir());
        final File fileTarget = new File(folderSource, filename);
        final ZipAppender appender = new ZipAppender(fileTarget);
        if (isEnabledCreateZip) {
            boolean success = appender.append(comment, grouping.toArray(new File[grouping.size()]));
            logger.finest("" + success);
            if (success) {
                for (File file : grouping) {
                    success &= FileU.delete(file);
                }
            }
            logger.finest("" + success);
        }
    }

    @Test
    public void testCategorize() throws Exception {
        final FileRegrouper fileRegrouper = new FileRegrouper(DurationU.Const.ONE_HOUR);
        final File filePath = new File(SystemU.tempDir());
        final Collection<File> files = new FindInFolderQuery(
                filePath.getParentFile(), filePath.getName(), false).getFound();
        for (File file : files) {
            fileRegrouper.add(file);
        }
        final Map<Date, Collection<File>> groupings = fileRegrouper.getGroupings();
        for (final Map.Entry<Date, Collection<File>> entry : groupings.entrySet()) {
            final Date dateEntry = entry.getKey();
            final Collection<File> grouping = entry.getValue();
            doGrouping(dateEntry, grouping);
        }
    }
}
