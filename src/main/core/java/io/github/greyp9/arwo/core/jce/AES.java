package io.github.greyp9.arwo.core.jce;

public final class AES {

    private AES() {
    }

    public static class Const {
        public static final String ALGORITHM = "AES";

        public static final int KEY_BITS = 256;
        public static final int KEY_BYTES = (KEY_BITS / Byte.SIZE);
    }
}
