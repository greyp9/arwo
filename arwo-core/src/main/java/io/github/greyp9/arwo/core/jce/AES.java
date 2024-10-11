package io.github.greyp9.arwo.core.jce;

import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import java.security.NoSuchAlgorithmException;

public final class AES {

    private AES() {
    }

    public static SecretKey generate() throws NoSuchAlgorithmException {
        final KeyGenerator keyGenerator = KeyGenerator.getInstance(AES.Const.ALGORITHM);
        keyGenerator.init(AES.Const.KEY_BITS);
        return keyGenerator.generateKey();
    }

    public static class Const {
        public static final String ALGORITHM = "AES";

        public static final int KEY_BITS = 256;
        public static final int KEY_BYTES = (KEY_BITS / Byte.SIZE);

        public static final int IV_BYTES_CTR = 16;
        public static final int IV_BYTES_GCM = 12;
        public static final int TAG_BYTES_GCM = 16;
    }
}
