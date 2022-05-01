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

    @Test
    public void test_OneSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env0.txt");
        final File fileShares = new File(folderTest, "env0.xml");
        final String expression =
                "secret(2 prop('file.encoding') prop('java.runtime.version'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));

        fileExpression.deleteOnExit();
        fileShares.deleteOnExit();
    }

    @Test
    public void test_OneSecretThreshold() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env2.txt");
        final File fileShares = new File(folderTest, "env2.xml");
        final String expression =
                "secret(2 prop('file.encoding') prop('java.runtime.version') prop('A'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));
        final byte[] secret = AES.generate().getEncoded();

        System.setProperty("A", "foo");
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);

        System.setProperty("A", "bar");
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));

        fileExpression.deleteOnExit();
        fileShares.deleteOnExit();
    }

    @Test
    public void test_OneSecretThresholdOrder() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env4.txt");
        final File fileShares = new File(folderTest, "env4.xml");
        final String expression =
                "secret(2 prop('A') prop('file.encoding') prop('java.runtime.version'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));
        final byte[] secret = AES.generate().getEncoded();

        System.setProperty("A", "foo");
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);

        System.setProperty("A", "bar");
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));

        fileExpression.deleteOnExit();
        fileShares.deleteOnExit();
    }

    @Test
    public void test_OneSecretThresholdFail() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env3.txt");
        final File fileShares = new File(folderTest, "env3.xml");
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

        fileExpression.deleteOnExit();
        fileShares.deleteOnExit();
    }

    @Test
    public void test_LayeredSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env1.txt");
        final File fileShares = new File(folderTest, "env1.xml");
        final String expression = "secret(2\n"
                + "secret(2 prop('file.encoding') prop('java.runtime.version'))\n"
                + "secret(2 prop('os.name') prop('os.version'))\n"
                + ")";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));

        fileExpression.deleteOnExit();
        fileShares.deleteOnExit();
    }
}
