package io.github.greyp9.arwo.core.i18n.test;

import io.github.greyp9.arwo.core.i18n.PseudoI18n;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.net.URL;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Logger;

public class Pseudo2Test {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testPartialHandling() throws Exception {
        final URL url = ResourceU.resolve("io/github/greyp9/arwo/properties/i18n/test.properties");
        Assert.assertNotNull(url);
        final File file = URLCodec.toFile(url);
        PseudoI18n.main(new String[] { "+", file.getAbsolutePath() });
        final Locale localeDF = Locale.getDefault();
        final Locale localeES = new Locale("ES");
        final ResourceBundle bundleDF = ResourceBundle.getBundle("io.github.greyp9.arwo.properties.i18n.test", localeDF);
        final ResourceBundle bundleES = ResourceBundle.getBundle("io.github.greyp9.arwo.properties.i18n.test", localeES);
        final String textDF = bundleDF.getString("table.sftpFolderType.footer");
        final String textES = bundleES.getString("table.sftpFolderType.footer");
        PseudoI18n.main(new String[] { "-", file.getAbsolutePath() });
        logger.finest(textDF);
        logger.finest(textES);
        Assert.assertEquals("[{0,number,integer} file(s)] [{1,number,integer} folder(s)]", textDF);
        Assert.assertEquals("[{0,number,integer} f\u00edl\u00e9(s)] [{1,number,integer} f\u00f3ld\u00e9r(s)]", textES);
    }
}
