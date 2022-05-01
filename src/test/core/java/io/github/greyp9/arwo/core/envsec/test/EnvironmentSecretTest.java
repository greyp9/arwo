package io.github.greyp9.arwo.core.envsec.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.envsec.EnvironmentSecret;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Arrays;
import java.util.Random;
import java.util.logging.Logger;

public class EnvironmentSecretTest {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final File folderTest = new File(SystemU.tempDir(), getClass().getSimpleName());

    @Before
    public void setUp() throws Exception {
        final boolean mkdirs = folderTest.mkdirs();
        logger.finest(String.format("mkdirs=%s", mkdirs));
    }

    /**
     * Custom cleanup per test case.
     */
    private void tearDownCustom(final File... files) {
        Arrays.stream(files).forEach(File::deleteOnExit);
    }

    @Test
    public void test_OneSecretProp() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envOSP.txt");
        final File fileShares = new File(folderTest, "envOSP.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression =
                "secret(2 prop('file.encoding') prop('java.runtime.version'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_OneSecretNewProp() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envOSNP.txt");
        final File fileShares = new File(folderTest, "envOSNP.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression =
                "secret(2 prop('file.encoding') prop('foo'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        System.setProperty("foo", "bar");  // add property that was missing, should cause recovery to fail
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertNotEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_OneSecretEnv() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envOSE.txt");
        final File fileShares = new File(folderTest, "envOSE.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression =
                "secret(2 env('HOME') env('SHELL'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_OneSecretMod() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envOSM.txt");
        final File fileShares = new File(folderTest, "envOSM.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression =
                "secret(2 mod('~') mod('~/Downloads'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_OneSecretPropWrap() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envOSPW.txt");
        final File fileShares = new File(folderTest, "envOSPW.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression =
                "secret(2 prop('file.encoding') mod(prop('user.home')))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_OneSecretThreshold() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envOST.txt");
        final File fileShares = new File(folderTest, "envOST.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression =
                "secret(2 prop('file.encoding') prop('java.runtime.version') prop('A'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));
        final byte[] secret = AES.generate().getEncoded();

        System.setProperty("A", "foo");
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);

        System.setProperty("A", "bar");
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_OneSecretThresholdOrder() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envOSTO.txt");
        final File fileShares = new File(folderTest, "envOSTO.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression =
                "secret(2 prop('A') prop('file.encoding') prop('java.runtime.version'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));
        final byte[] secret = AES.generate().getEncoded();

        System.setProperty("A", "foo");
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);

        System.setProperty("A", "bar");
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_OneSecretThresholdFail() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envOSTF.txt");
        final File fileShares = new File(folderTest, "envOSTF.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression =
                "secret(2 prop('file.encoding') prop('A') prop('B'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));
        final byte[] secret = AES.generate().getEncoded();

        System.setProperty("A", "foo");
        System.setProperty("B", "bar");
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);

        System.setProperty("A", "bar");
        System.setProperty("B", "foo");
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertNotEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }

    @Test
    public void test_LayeredSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "envLS.txt");
        final File fileShares = new File(folderTest, "envLS.txt.xml");
        tearDownCustom(fileExpression, fileShares);
        final String expression = "secret(2\n"
                + "secret(2 prop('file.encoding') prop('java.runtime.version'))\n"
                + "secret(2 prop('os.name') prop('os.version'))\n"
                + ")";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));
    }
}
