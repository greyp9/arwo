package io.github.greyp9.arwo.core.i18n.test;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PseudoTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testUnicodeSequenceHandling() throws Exception {
        final String input = "\\ud83c\\uddfa\\ud83c\\uddf8 EN";
        logger.finest(input);
        final Pattern unicodeSequence = Pattern.compile("\\\\u\\p{XDigit}{4}");
        final Matcher matcher = unicodeSequence.matcher(input);
        final boolean find = matcher.find();
        Assertions.assertTrue(find);
        final String group = matcher.group(0);
        Assertions.assertEquals("\\ud83c", group);
    }

    @Test
    public void testUnicodeSequenceHandlingAlt() throws Exception {
        final String input = "\\ud83c\\uddfa\\ud83c\\uddf8 EN";
        logger.finest(input);
        final Pattern unicodeSequence = Pattern.compile("\\\\u[0-9a-fA-F]{4}");
        final Matcher matcher = unicodeSequence.matcher(input);
        final boolean find = matcher.find();
        Assertions.assertTrue(find);
        final String group = matcher.group(0);
        Assertions.assertEquals("\\ud83c", group);
    }

/*
    public void testName3() throws Exception {  // trace through PseudoI18n invocation
        final URL urlXSD = ResourceU.resolve(App.Actions.XSD);
        logger.info(urlXSD.toExternalForm());
        final File fileXSD = URLCodec.toFile(urlXSD);
        final String filenameProperties = fileXSD.getName().replace(".xsd", ".properties");
        final File fileProperties = new File(fileXSD.getParentFile(), filenameProperties);
        new PseudoI18n().pseudoLocalize("+", fileProperties);
    }
*/
}
