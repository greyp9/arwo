package io.github.greyp9.arwo.core.file.fs.test;

import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class FileSystemTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testFileAttributesLinux() {
        if (SystemU.isLinux()) {
            File fileBashHistory = new File(SystemU.resolve("~/.bash_history"));
            File fileBinJava = new File("/usr/bin/java");
            File fileLibJava = new File("/usr/lib/jvm");
            //
            Assert.assertTrue(fileBashHistory.isFile());
            Assert.assertFalse(fileBashHistory.isDirectory());
            Assert.assertFalse(FileU.isLink(fileBashHistory));
            //
            Assert.assertTrue(fileBinJava.isFile());
            Assert.assertFalse(fileBinJava.isDirectory());
            Assert.assertTrue(FileU.isLink(fileBinJava));
            //
            Assert.assertFalse(fileLibJava.isFile());
            Assert.assertTrue(fileLibJava.isDirectory());
            Assert.assertFalse(FileU.isLink(fileLibJava));
        }
    }

    @Test
    public void testSymlinkLinux() {
        File fileIt = new File("/usr/bin/java");
        //Assert.assertTrue(FileU.isLink(fileIt));

        final Collection<File> files = new TreeSet<>();
        files.add(fileIt);
        while (FileU.isLink(fileIt)) {
            fileIt = FileU.getCanonicalFile(fileIt);
            files.add(fileIt);
            logger.finest(fileIt.getAbsolutePath());
        }
        //Assert.assertEquals(2, files.size());
        for (File file : files) {
            logger.finest(file.getAbsolutePath());
        }
    }

    @Test
    public void testEnumerateFolder() {
        final String[] folderPaths = {
                SystemU.userHome(),
                SystemU.tempDir(),
        };
        for (String folderPath : folderPaths) {
            final File folder = new File(folderPath);
            Assert.assertTrue(folder.exists());
            final File[] files = FileU.listFiles(folder);
            for (File file : files) {
                logger.finest(file.getAbsolutePath());
                Assert.assertTrue(file.exists());
            }
        }
    }

    @Test
    public void testEnumerateFolderRetainRecent() {
        final String[] folderPaths = {
                SystemU.userHome(),
                SystemU.tempDir(),
        };
        final int recentCount = 10;
        for (String folderPath : folderPaths) {
            final File folder = new File(folderPath);
            Assert.assertTrue(folder.exists());
            final File[] files = FileU.listFiles(folder);
            final long skip = Math.max(0, (files.length - recentCount));
            final List<File> filesRecent = Arrays.stream(files).sorted().skip(skip).collect(Collectors.toList());
            Assert.assertTrue(filesRecent.size() <= recentCount);
        }
    }
}
