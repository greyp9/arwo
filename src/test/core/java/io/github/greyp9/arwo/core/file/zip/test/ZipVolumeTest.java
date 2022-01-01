package io.github.greyp9.arwo.core.file.zip.test;

import io.github.greyp9.arwo.core.date.DateConvertU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.file.zip.ZipMetaData;
import io.github.greyp9.arwo.core.file.zip.ZipVolume;
import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

public class ZipVolumeTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    @Test
    public void testFindIterateArchives() throws Exception {
        File root = new File(SystemU.resolve("~/Downloads"));
        Collection<File> files = new FindInFolderQuery(root, "*.zip", false).getFound();
        logger.info("" + files.size());
        for (File file : files) {
            logger.info("" + file.getPath());
            checkFile(file);
        }
    }

    private void checkFile(File file) throws IOException {
        logger.finest(StringU.create(80, "-"));
        logger.finest(String.format(Const.PATTERN_ENTRY,
                file.length(), new Date(file.lastModified()), file.getAbsolutePath()));
        logger.finest(StringU.create(40, "-"));
        final ZipVolume zipVolume = new ZipVolume(file);
        final Collection<ZipMetaData> entries = zipVolume.getEntries();
        for (ZipMetaData metaData : entries) {
            logger.finest(String.format(Const.PATTERN_ENTRY, metaData.getLength(),
                    XsdDateU.toXSDZMillis(DateConvertU.fromMillis(metaData.getLastModified())), metaData.getPath()));
        }
    }

    private static class Const {
        private static final String PATTERN_ENTRY = "%10d  %s  %s";
    }
}
