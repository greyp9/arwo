package io.github.greyp9.arwo.core.i18n;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PseudoI18n {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public static void main(final String[] args) throws Exception {
        if (args.length < 2) {
            throw new IllegalArgumentException("PseudoI18n [+|-] [property-file-URL]");
        } else if (args[0].equals("+")) {
            new PseudoI18n().generate(args[0], Arrays.asList(args));
        } else if (args[0].equals("-")) {
            new PseudoI18n().generate(args[0], Arrays.asList(args));
        } else {
            throw new IllegalArgumentException("PseudoI18n [+|-] [property-file-URL]");
        }
    }

    private void generate(final String operation, final Collection<String> paths) throws IOException {
        final Iterator<String> iterator = paths.iterator();
        iterator.next();  // skip operation (arg[0])
        while (iterator.hasNext()) {
            generate(operation, iterator.next());
        }
    }

    private void generate(final String operation, final String path) throws IOException {
        final File file = new File(path);
        final Level level = (file.exists() ? Level.INFO : Level.SEVERE);
        logger.log(level, path);
        if (file.exists()) {
            pseudoLocalize(operation, file);
        }
    }

    private void pseudoLocalize(final String operation, final File fileSource) throws IOException {
        // filesystem
        final File folder = fileSource.getParentFile();
        final String nameJA = fileSource.getName().replace(".properties", "_ja.properties");
        final String nameRU = fileSource.getName().replace(".properties", "_ru.properties");
        final String nameDE = fileSource.getName().replace(".properties", "_de.properties");
        final String nameFR = fileSource.getName().replace(".properties", "_fr.properties");
        final String nameES = fileSource.getName().replace(".properties", "_es.properties");
        if ("+".equals(operation)) {
            pseudoLocalize(fileSource, new File(folder, nameJA), createLookupTableJA());
            pseudoLocalize(fileSource, new File(folder, nameRU), createLookupTableRU());
            pseudoLocalize(fileSource, new File(folder, nameDE), createLookupTableDE());
            pseudoLocalize(fileSource, new File(folder, nameFR), createLookupTableFR());
            pseudoLocalize(fileSource, new File(folder, nameES), createLookupTableES());
        } else if ("-".equals(operation)) {
            FileU.delete(new File(folder, nameJA));
            FileU.delete(new File(folder, nameRU));
            FileU.delete(new File(folder, nameDE));
            FileU.delete(new File(folder, nameFR));
            FileU.delete(new File(folder, nameES));
        }
    }

    private void pseudoLocalize(
            final File fileSource, final File fileTarget, final Properties properties) throws IOException {
        if (fileSource.exists()) {
            final String message = String.format("FROM:[%s] TO:[%s]", fileSource.getName(), fileTarget.getName());
            logger.info(Value.join("/", message, "[START]"));
            final byte[] bytesSource = StreamU.read(fileSource);
            final String source = UTF8Codec.toString(bytesSource);
            final String target = pseudoLocalize(source, properties);
            final byte[] bytesTarget = UTF8Codec.toBytes(target);
            StreamU.write(fileTarget, bytesTarget);
            //noinspection ConstantConditions
            logger.info(Value.join("/", message, "[FINISH]", bytesSource.length, bytesTarget.length));
        }
    }

    private String pseudoLocalize(final String source, final Properties properties) throws IOException {
        final Pattern pattern = Pattern.compile("(?m)^(.+)\\s*=\\s*(.+)$");
        final StringWriter stringWriter = new StringWriter();
        final PrintWriter printWriter = new PrintWriter(stringWriter);
        final BufferedReader reader = new BufferedReader(new StringReader(source));
        boolean moreData = true;
        while (moreData) {
            String line = reader.readLine();
            if (line == null) {
                moreData = false;
            } else {
                final Matcher matcher = pattern.matcher(line);
                if (matcher.matches()) {
                    final String valueOld = matcher.group(2);
                    final String valueNew = pseudoLocalizeValue(valueOld, properties);
                    line = line.substring(0, matcher.start(2)) + valueNew;
                }
                printWriter.println(line);
            }
        }
        return stringWriter.toString();
    }

    private String pseudoLocalizeValue(final String valueOld, final Properties properties) {
        final StringBuilder buffer = new StringBuilder();
        for (char c : valueOld.toCharArray()) {
            final String characterOld = new String(new char[] { c });
            final String characterNew = properties.getProperty(characterOld, characterOld);
            buffer.append(characterNew);
        }
        return buffer.toString();
    }

    private static Properties createLookupTableES() {
        Properties properties = new Properties();
        properties.setProperty("A", "\\u00c1");
        properties.setProperty("E", "\\u00c9");
        properties.setProperty("I", "\\u00cd");
        properties.setProperty("N", "\\u00d1");
        properties.setProperty("O", "\\u00d3");
        properties.setProperty("U", "\\u00da");
        properties.setProperty("a", "\\u00e1");
        properties.setProperty("e", "\\u00e9");
        properties.setProperty("i", "\\u00ed");
        properties.setProperty("n", "\\u00f1");
        properties.setProperty("o", "\\u00f3");
        properties.setProperty("u", "\\u00fa");
        return properties;
    }

    private static Properties createLookupTableFR() {
        Properties properties = new Properties();
        properties.setProperty("A", "\\u00c2");
        properties.setProperty("C", "\\u00c7");
        properties.setProperty("E", "\\u00ca");
        properties.setProperty("I", "\\u00ce");
        properties.setProperty("O", "\\u00d4");
        properties.setProperty("U", "\\u00db");
        properties.setProperty("a", "\\u00e2");
        properties.setProperty("c", "\\u00e7");
        properties.setProperty("e", "\\u00ea");
        properties.setProperty("i", "\\u00ee");
        properties.setProperty("o", "\\u00f4");
        properties.setProperty("u", "\\u00fb");
        return properties;
    }

    private static Properties createLookupTableDE() {
        Properties properties = new Properties();
        properties.setProperty("A", "\\u00c4");
        properties.setProperty("O", "\\u00d6");
        properties.setProperty("U", "\\u00dc");
        properties.setProperty("B", "\\u00df");
        properties.setProperty("a", "\\u00e4");
        properties.setProperty("o", "\\u00f6");
        properties.setProperty("u", "\\u00fc");
        return properties;
    }

    private static Properties createLookupTableRU() {
        // https://en.wikipedia.org/wiki/Letter_frequency
        // (ETAOINSHRDLCUMWFGYPBVKJXQZ) (top 12: 80%)
        // https://en.wikipedia.org/wiki/List_of_Unicode_characters
        Properties properties = new Properties();
        // uppercase
        properties.setProperty("E", "\\u0417");
        properties.setProperty("T", "\\u0422");
        properties.setProperty("A", "\\u0410");
        properties.setProperty("O", "\\u041e");
        properties.setProperty("I", "\\u0407");
        properties.setProperty("N", "\\u0419");
        properties.setProperty("S", "\\u0405");
        properties.setProperty("H", "\\u041d");
        properties.setProperty("R", "\\u042f");
        properties.setProperty("D", "\\u0414");
        properties.setProperty("L", "\\u0413");
        properties.setProperty("C", "\\u0421");
        // lowercase
        properties.setProperty("e", "\\u0437");
        properties.setProperty("t", "\\u0442");
        properties.setProperty("a", "\\u0430");
        properties.setProperty("o", "\\u043e");
        properties.setProperty("i", "\\u0457");
        properties.setProperty("n", "\\u0439");
        properties.setProperty("s", "\\u0455");
        properties.setProperty("h", "\\u043d");
        properties.setProperty("r", "\\u044f");
        properties.setProperty("d", "\\u0434");
        properties.setProperty("l", "\\u0433");
        properties.setProperty("c", "\\u0441");
        return properties;
    }

    private static Properties createLookupTableJA() {
        // Turn ABC into kanji
        //   http://www.sljfaq.org/cgi/kanjiabc.cgi
        // Unicode code point lookup/search tool
        //   http://www.scarfboy.com/coding/unicode-tool
        Properties properties = new Properties();
        // uppercase
        properties.setProperty("E", "\\u30e8");  // KATAKANA LETTER YO
        properties.setProperty("T", "\\u535e");  // CJK UNIFIED IDEOGRAPH-535E
        properties.setProperty("A", "\\u4e39");  // CJK UNIFIED IDEOGRAPH-4E39
        properties.setProperty("O", "\\u56de");  // CJK UNIFIED IDEOGRAPH-56DE
        properties.setProperty("I", "\\u5de5");  // CJK UNIFIED IDEOGRAPH-5DE5
        properties.setProperty("N", "\\u51e0");  // CJK UNIFIED IDEOGRAPH-51E0
        properties.setProperty("S", "\\u5df1");  // CJK UNIFIED IDEOGRAPH-5DF1
        properties.setProperty("H", "\\u5efe");  // CJK UNIFIED IDEOGRAPH-5EFE
        properties.setProperty("R", "\\u5c3a");  // CJK UNIFIED IDEOGRAPH-5C3A
        properties.setProperty("D", "\\u53e5");  // CJK UNIFIED IDEOGRAPH-53E5
        properties.setProperty("L", "\\u3057");  // HIRAGANA LETTER SI
        properties.setProperty("C", "\\u4ea1");  // CJK UNIFIED IDEOGRAPH-4EA1
        // lowercase
        properties.setProperty("e", "\\u30e8");  // KATAKANA LETTER YO
        properties.setProperty("t", "\\u535e");  // CJK UNIFIED IDEOGRAPH-535E
        properties.setProperty("a", "\\u4e39");  // CJK UNIFIED IDEOGRAPH-4E39
        properties.setProperty("o", "\\u56de");  // CJK UNIFIED IDEOGRAPH-56DE
        properties.setProperty("i", "\\u5de5");  // CJK UNIFIED IDEOGRAPH-5DE5
        properties.setProperty("n", "\\u51e0");  // CJK UNIFIED IDEOGRAPH-51E0
        properties.setProperty("s", "\\u5df1");  // CJK UNIFIED IDEOGRAPH-5DF1
        properties.setProperty("h", "\\u5efe");  // CJK UNIFIED IDEOGRAPH-5EFE
        properties.setProperty("r", "\\u5c3a");  // CJK UNIFIED IDEOGRAPH-5C3A
        properties.setProperty("d", "\\u53e5");  // CJK UNIFIED IDEOGRAPH-53E5
        properties.setProperty("l", "\\u3057");  // HIRAGANA LETTER SI
        properties.setProperty("c", "\\u4ea1");  // CJK UNIFIED IDEOGRAPH-4EA1
        return properties;
    }
}
