package io.github.greyp9.arwo.core.i18n;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.TreeSet;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ScanI18n {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public static void main(final String[] args) throws Exception {
        if (args.length < 1) {
            throw new IllegalArgumentException("ScanI18n [source-folder]");
        } else {
            new ScanI18n().scan(args[0]);
        }
    }

    private final Collection<String> strings = new TreeSet<String>();

    private void scan(String folderPath) throws IOException {
        io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));

        File folder = new File(folderPath);
        logger.info(MessageFormat.format("[{1}][{0}]", folder, folder.exists()));
        Collection<File> files = new FindInFolderQuery(folder, "*.java", true).getFound();
        logger.info(String.format("[%,d]", files.size()));
        for (File file : files) {
            scanFile(file);
        }
        for (String string : strings) {
            logger.info(string);
        }
        logger.info(String.format("[%d]", strings.size()));
    }

    private void scanFile(File file) throws IOException {
        String source = UTF8Codec.toString(StreamU.read(file));
        if (source.contains("i18nf")) {
            return;
        }
        final BufferedReader reader = new BufferedReader(new StringReader(source));
        boolean moreData = true;
        while (moreData) {
            String line = reader.readLine();
            if (line == null) {
                moreData = false;
            } else {
                scanLine(file, line);
            }
        }
    }

    private void scanLine(File file, String line) {
        boolean ok = true;
        if (line.contains("i18n")) {
            return;
        }
        if (line.contains("@SuppressWarnings")) {
            return;
        }
        final Pattern pattern = Pattern.compile("\"(.*?)\"");
        final Matcher matcher = pattern.matcher(line);
        while (matcher.find()) {
            ok &= scanLiteralString(line, matcher.start(1), matcher.group(1));
            if (!ok) {
                strings.add(String.format("[%s][%s]", SystemU.unresolve(file.getPath()), matcher.group(1)));
                //strings.add(SystemU.unresolve(file.getPath()));
                //strings.add(matcher.group(1));
            }
        }
    }

/*
    @SuppressWarnings("unused")
    private void scanFileForStringLiterals(File file) throws IOException {
        logger.info(file.getAbsolutePath());
        final String source = UTF8Codec.toString(StreamU.read(file));
        final Pattern pattern = Pattern.compile("(?m)\"(.*)\"");
        final Matcher matcher = pattern.matcher(source);
        while (matcher.find()) {
            scanLiteralString(matcher.group(1));
        }
    }
*/

    private boolean scanLiteralString(String line, int start, String s) {
        boolean ok = false;
        if (s.equals("")) {
            ok = true;
        }
        if (s.contains(".")) {
            ok = true;
        }
        if (s.contains("%s")) {
            ok = true;
        }
        if (s.contains("%d")) {
            ok = true;
        }
        if (s.contains("checkstyle:")) {
            ok = true;
        }
        final int commentStart = line.indexOf("//");
        if ((commentStart >= 0) && (commentStart < start)) {
            ok = true;
        }
        return ok;
    }
}
