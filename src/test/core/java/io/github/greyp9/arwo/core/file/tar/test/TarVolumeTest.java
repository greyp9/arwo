package io.github.greyp9.arwo.core.file.tar.test;

import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.file.tar.TarMetaData;
import io.github.greyp9.arwo.core.file.tar.TarVolume;
import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.lang.SystemU;
import junit.framework.TestCase;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

public class TarVolumeTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    public void testFindIterateArchives() throws Exception {
        File root = new File(SystemU.resolve("~/Downloads"));
        Collection<File> files = new TreeSet<File>();
        files.addAll(new FindInFolderQuery(root, "*.tar.gz", true).getFound());
        files.addAll(new FindInFolderQuery(root, "*.tgz", true).getFound());
        logger.info("" + files.size());
        for (File file : files) {
            checkFile(file);
        }
    }

    private void checkFile(File file) throws IOException {
        Level level = Level.FINEST;
        logger.info(StringU.create(80, "-"));
        logger.info(String.format(Const.PATTERN_ENTRY,
                file.length(), new Date(file.lastModified()), file.getAbsolutePath()));
        logger.log(level, StringU.create(40, "-"));
        final TarVolume tarVolume = new TarVolume(file);
        final Collection<TarMetaData> entries = tarVolume.getEntries();
        for (TarMetaData metaData : entries) {
            logger.log(level, String.format(Const.PATTERN_ENTRY,
                    metaData.getLength(), metaData.getLastModified(), metaData.getPath()));
        }
    }

    private static class Const {
        private static final String PATTERN_ENTRY = "%10d  %s  %s";
    }
}
