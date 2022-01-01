package io.github.greyp9.arwo.core.file.zip.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.meta.MetaFileFactory;
import io.github.greyp9.arwo.core.file.zip.ZipAppender;
import io.github.greyp9.arwo.core.file.zip.ZipMetaData;
import io.github.greyp9.arwo.core.file.zip.ZipVolume;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.Assert;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;

public class ZipAppenderTest {

    @Test
    public void testCreate() throws Exception {
        // setup
        final Date date = DateU.floor(new Date(), "PT2S");  // time resolution recorded in zip
        final File folderTemp = new File(SystemU.tempDir());
        Assert.assertTrue(folderTemp.exists());
        final String filename = String.format("temp-test-%s.zip", DateX.toFilename(date));
        final File fileTemp = new File(folderTemp, filename);
        Assert.assertFalse(fileTemp.exists());
        // append one
        final MetaFile metaFileA = MetaFileFactory.create("a.txt", date.getTime(), UTF8Codec.toBytes("abc123"));
        if (SystemU.isTrue()) {
            final ZipAppender zipAppender = new ZipAppender(fileTemp);
            final boolean success = zipAppender.append("1", metaFileA);
            Assert.assertTrue(success);
        }
        validateFile(fileTemp, 1, metaFileA, null);
        validateBytes(fileTemp, 1, metaFileA, null);
        // append two
        final MetaFile metaFileB = MetaFileFactory.create("b.txt", date.getTime(), UTF8Codec.toBytes("bbc123"));
        final MetaFile metaFileC = MetaFileFactory.create("c.txt", date.getTime(), UTF8Codec.toBytes("ccc123"));
        if (SystemU.isTrue()) {
            final ZipAppender zipAppender = new ZipAppender(fileTemp);
            final boolean success = zipAppender.append("2", metaFileB, metaFileC);
            Assert.assertTrue(success);
        }
        validateFile(fileTemp, 3, metaFileA, metaFileB);
        validateBytes(fileTemp, 3, metaFileA, metaFileB);
    }

    private static void validateFile(
            File fileTemp, int size, MetaFile metaFileA, MetaFile metaFileB) throws IOException {
        final ZipVolume zipVolume = new ZipVolume(fileTemp);
        final MetaFile metaFileNull = zipVolume.getEntry(null);
        Assert.assertNull(metaFileNull);
        final Collection<ZipMetaData> entries = zipVolume.getEntries();
        for (ZipMetaData entry : entries) {
            final String comment = entry.getComment();
            if (entry.getPath().equals("a.txt")) {
                Assert.assertEquals("1", comment);
            } else if (entry.getPath().equals("b.txt")) {
                Assert.assertEquals("2", comment);
            } else if (entry.getPath().equals("c.txt")) {
                Assert.assertEquals("2", comment);
            }
        }
        Assert.assertEquals(size, entries.size());
        // validate file entry
        final MetaFile metaFileALoad = zipVolume.getEntry("a.txt");
        Assert.assertNotNull(metaFileALoad);
        Assert.assertEquals(metaFileA.getMetaData().toString(), metaFileALoad.getMetaData().toString());
        if (metaFileB != null) {
            final MetaFile metaFileBLoad = zipVolume.getEntry("b.txt");
            Assert.assertNotNull(metaFileBLoad);
            Assert.assertEquals(metaFileB.getMetaData().toString(), metaFileBLoad.getMetaData().toString());
        }
    }

    private static void validateBytes(
            File fileTemp, int size, MetaFile metaFileA, MetaFile metaFileB) throws IOException {
        final byte[] bytesTemp = StreamU.read(fileTemp);
        final ZipVolume zipVolume = new ZipVolume(new ByteArrayInputStream(bytesTemp));
        final MetaFile metaFileNull = zipVolume.getEntry(null);
        Assert.assertNull(metaFileNull);
        final Collection<ZipMetaData> entries = zipVolume.getEntries();
        for (ZipMetaData entry : entries) {
            final String comment = entry.getComment();
            //
            // comment doesn't work for ZipInputStream usage
            //
            if (entry.getPath().equals("a.txt")) {
                Assert.assertEquals(null, comment);
            } else if (entry.getPath().equals("b.txt")) {
                Assert.assertEquals(null, comment);
            } else if (entry.getPath().equals("c.txt")) {
                Assert.assertEquals(null, comment);
            }
        }
        Assert.assertEquals(size, entries.size());
        // validate file entry
        final MetaFile metaFileALoad = zipVolume.getEntry("a.txt");
        Assert.assertNotNull(metaFileALoad);
        Assert.assertEquals(metaFileA.getMetaData().toString(), metaFileALoad.getMetaData().toString());
        if (metaFileB != null) {
            final MetaFile metaFileBLoad = zipVolume.getEntry("b.txt");
            Assert.assertNotNull(metaFileBLoad);
            Assert.assertEquals(metaFileB.getMetaData().toString(), metaFileBLoad.getMetaData().toString());
        }
    }
}
