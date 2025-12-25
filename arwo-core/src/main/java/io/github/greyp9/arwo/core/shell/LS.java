package io.github.greyp9.arwo.core.shell;

import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFolder;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.text.line.LineU;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class LS {
    private static final String REGEX_LS_ENTRY
            = "(.{10,11})\\s+(\\d+)\\s+(\\w+)\\s+(\\w+)\\s+(\\d+)\\s(.{25})\\s(\\S+)(\\s.*)?";
    private static final String TYPE_DIRECTORY = "d";
    private static final String TYPE_LINK = "l";
    private static final int INDEX_PERMS = 1;
    private static final int INDEX_OWNER = 3;
    private static final int INDEX_GROUP = 4;
    private static final int INDEX_SIZE = 5;
    private static final int INDEX_MTIME = 6;
    private static final int INDEX_NAME = 7;
    private static final int INDEX_EXTRA = 8;

    public MetaFolder toMetaFolder(final String location, final String ls) throws IOException {
        final List<FileMetaData> files = new ArrayList<>();
        final Pattern pattern = Pattern.compile(REGEX_LS_ENTRY);
        final Collection<String> lines = LineU.toLines(ls);
        for (String line : lines) {
            final Matcher matcher = pattern.matcher(line);
            if (matcher.matches()) {
                final String perms = matcher.group(INDEX_PERMS);
                final FileMetaData.Type type = toType(perms);
                final Long size = NumberU.toLong(matcher.group(INDEX_SIZE), 0L);
                final Date date = XsdDateU.fromISO(matcher.group(INDEX_MTIME));
                final Long mtime = date == null ? null : date.getTime();
                final String name = matcher.group(INDEX_NAME);
                final FileMetaData fileMetaData = new FileMetaData(name, size, mtime, 0L, type);
                fileMetaData.setProperty(FileMetaData.PERMS, perms);
                fileMetaData.setProperty(FileMetaData.OWNER, matcher.group(INDEX_OWNER));
                fileMetaData.setProperty(FileMetaData.GROUP, matcher.group(INDEX_GROUP));
                if (FileMetaData.Type.LINK.equals(type)) {
                    fileMetaData.setProperty(FileMetaData.LINK, matcher.group(INDEX_EXTRA).replace(" -> ", ""));
                }

                files.add(fileMetaData);
            }
        }
        return new MetaFolder(location, files);
    }

    private FileMetaData.Type toType(final String perms) {
        return (perms.startsWith(TYPE_DIRECTORY)) ? FileMetaData.Type.DIRECTORY
                : (perms.startsWith(TYPE_LINK)) ? FileMetaData.Type.LINK
                : FileMetaData.Type.FILE;
    }
}
