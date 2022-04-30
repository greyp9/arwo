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

public class EnvironmentSecretTest {
    private final File folderTest = new File(SystemU.tempDir(), getClass().getSimpleName());

    @Before
    public void setUp() throws Exception {
        folderTest.mkdirs();
    }

    @Test
    public void test_OneSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env0.txt");
        final File fileShares = new File(folderTest, "env0.xml");
        final String expression =
                "secret(prop('file.encoding') prop('java.runtime.version'))";
        StreamU.write(fileExpression, UTF8Codec.toBytes(expression));

        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(fileExpression.getPath(), new Random(0L)).generate(secret);
        final byte[] secretRecover = new EnvironmentSecret(fileExpression.getPath(), null).recover();
        Assert.assertEquals(HexCodec.encode(secret), HexCodec.encode(secretRecover));

        fileExpression.deleteOnExit();
        fileShares.deleteOnExit();
    }

    @Test
    public void test_LayeredSecret() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(folderTest, "env1.txt");
        final File fileShares = new File(folderTest, "env1.xml");
        final String expression = "secret(\n"
                + "secret(prop('file.encoding') prop('java.runtime.version'))\n"
                + "secret(prop('os.name') prop('os.version'))\n"
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
