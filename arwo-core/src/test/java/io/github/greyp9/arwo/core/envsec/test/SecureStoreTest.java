package io.github.greyp9.arwo.core.envsec.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.envsec.EnvironmentSecret;
import io.github.greyp9.arwo.core.envsec.store.SecureStore;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import javax.crypto.AEADBadTagException;
import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Arrays;
import java.util.Random;
import java.util.UUID;

public class SecureStoreTest {
    private static final File FOLDER_TEST = new File(SystemU.tempDir(), SecureStoreTest.class.getSimpleName());
    private static final String FILENAME_EXPRESSION = String.format("%s.txt", UUID.randomUUID());
    private static final String FILENAME_SHARES = String.format("%s.xml", FILENAME_EXPRESSION);
    private static final File FILE_EXPRESSION = new File(FOLDER_TEST, FILENAME_EXPRESSION);
    private static final File FILE_SHARES = new File(FOLDER_TEST, FILENAME_SHARES);

    @BeforeAll
    static void beforeAll() throws IOException, GeneralSecurityException {
        FileU.ensureFolder(FOLDER_TEST);
        tearDownCustom(FILE_EXPRESSION, FILE_SHARES);

        final String expression =
                "secret(2 prop('file.encoding') prop('java.runtime.version'))";
        StreamU.write(FILE_EXPRESSION, UTF8Codec.toBytes(expression));
        final byte[] secret = AES.generate().getEncoded();
        new EnvironmentSecret(FILE_EXPRESSION.getPath(), new Random(0L)).generate(secret);
    }

    static void tearDownCustom(final File... files) {
        Arrays.stream(files).forEach(File::deleteOnExit);
    }

    @Test
    void testSecureStore_HappyPath() throws IOException {
        final SecureStore secureStore = new SecureStore(FILE_EXPRESSION);
        Assertions.assertNull(secureStore.getException());

        final String string = UUID.randomUUID().toString();
        secureStore.setPropertyProtect(string, string);
        Assertions.assertEquals(string, secureStore.getProperty(string));
    }

    @Test
    void testSecureStore_MissingValue() throws IOException {
        final SecureStore secureStore = new SecureStore(FILE_EXPRESSION);
        Assertions.assertNull(secureStore.getException());

        final String string = UUID.randomUUID().toString();
        Assertions.assertNull(secureStore.getProperty(string));
    }

    @Test
    void testSecureStore_DecodeFailure() {
        final SecureStore secureStore = new SecureStore(FILE_EXPRESSION);
        Assertions.assertNull(secureStore.getException());

        final String string = UUID.randomUUID().toString();
        secureStore.setProperty(string, string);
        final IllegalArgumentException exception = Assertions.assertThrows(
                IllegalArgumentException.class, () -> secureStore.getProperty(string));
        Assertions.assertTrue(exception.getMessage().contains("Illegal base64 character"));
    }

    @Test
    void testSecureStore_DecryptFailure() {
        final SecureStore secureStore = new SecureStore(FILE_EXPRESSION);
        Assertions.assertNull(secureStore.getException());

        final String string = UUID.randomUUID().toString();
        secureStore.setProperty(string, Base64Codec.encode(UTF8Codec.toBytes(string)));
        final IOException exception = Assertions.assertThrows(
                IOException.class, () -> secureStore.getProperty(string));
        Assertions.assertInstanceOf(AEADBadTagException.class, exception.getCause());
    }
}
