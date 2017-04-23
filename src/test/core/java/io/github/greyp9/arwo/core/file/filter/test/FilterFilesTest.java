package io.github.greyp9.arwo.core.file.filter.test;

import io.github.greyp9.arwo.core.file.filter.FilterFiles;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.lang.SystemU;
import junit.framework.TestCase;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

public class FilterFilesTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testByAgeMin() throws Exception {
        final Date date = new Date();
        final File folderTemp = new File(SystemU.tempDir());
        final Collection<File> found = new FindInFolderQuery(folderTemp, "*", true).getFound();
        int minutes = 1;
        while (true) {
            final Collection<File> foundCopy = new ArrayList<File>(found);
            final String ageMin = String.format("PT%dM", minutes);
            FilterFiles.byAgeMin(foundCopy, date, ageMin);
            logger.fine(String.format("[%s][%d]", ageMin, foundCopy.size()));
            for (File file : foundCopy) {
                logger.finest(file.getName());
            }
            if (foundCopy.isEmpty()) {
                break;
            } else {
                minutes *= 2;
            }
        }
    }
}
