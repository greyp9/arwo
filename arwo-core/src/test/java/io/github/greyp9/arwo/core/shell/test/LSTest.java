package io.github.greyp9.arwo.core.shell.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFolder;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.shell.LS;
import io.github.greyp9.arwo.core.text.line.LineU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;

public class LSTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testListing1() throws IOException {
        final String resource = "io/github/greyp9/arwo/core/shell/ls1.txt";
        final String text = UTF8Codec.toString(StreamU.read(ResourceU.resolve(resource)));
        Assertions.assertFalse(text.isEmpty());
        final LS converter = new LS();
        final MetaFolder metaFolder = converter.toMetaFolder(Http.Token.SLASH, text);
        Assertions.assertEquals(LineU.toLines(text).size() - 1, metaFolder.getFiles().size());
        final List<FileMetaData> files = metaFolder.getFiles();
        for (FileMetaData file : files) {
            logger.finest(String.format("NAME:[%s] SIZE:[%d] DIR:[%s]",
                    file.getPath(), file.getLength(), file.isDirectory()));
        }
    }
}
