package io.github.greyp9.arwo.core.file.zip.test;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.file.group.FileGrouper;
import io.github.greyp9.arwo.core.file.zip.ZipAppender;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.logging.Logger;

public class ZipArchiveTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testCategorize() throws Exception {
        boolean isEnabledCreateZip = System.getProperties().isEmpty();
        final String comment = XsdDateU.toXSDZMillis(new Date());
        // source files
        final File folderSource = new File(SystemU.tempDir());
        logger.finest(folderSource.getAbsolutePath());
        Collection<File> files = new FindInFolderQuery(folderSource, "*.*", false).getFound();
        logger.finest("" + files.size());
        // group files
        FileGrouper fileGrouper = new FileGrouper("PT1H");
        for (File file : files) {
            fileGrouper.add(file);
        }
        // results
        final Map<Date, Collection<File>> groupings = fileGrouper.getGroupings();
        for (final Map.Entry<Date, Collection<File>> entry : groupings.entrySet()) {
            final Date dateEntry = entry.getKey();
            final Collection<File> grouping = entry.getValue();
            // target file
            final String filename = String.format("%s.%s.zip",
                    getClass().getSimpleName(), DateX.toFilename(dateEntry));
            final File fileTarget = new File(folderSource, filename);
            ZipAppender appender = new ZipAppender(fileTarget);
            logger.finest("---------=---------=");
            logger.finest(XsdDateU.toXSDZ(dateEntry));
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
            // debug
            for (File file : grouping) {
                logger.finest(file.getName());
            }
        }
    }
}
