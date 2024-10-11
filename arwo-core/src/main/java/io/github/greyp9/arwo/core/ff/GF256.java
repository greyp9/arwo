package io.github.greyp9.arwo.core.ff;

import java.util.Random;

import static io.github.greyp9.arwo.core.ff.FiniteField.EXP;
import static io.github.greyp9.arwo.core.ff.FiniteField.LOG;

public final class GF256 {

    private GF256() {
    }

    public static byte[] generate(final Random random, final int degree, final byte data) {
        final byte[] bytes = new byte[degree + 1];
        do {
            random.nextBytes(bytes);
        } while (bytes[bytes.length - 1] == 0);
        bytes[0] = data;
        return bytes;
    }

    public static byte evaluate(final byte[] bytes, final byte data) {
        byte result = 0;
        for (int i = bytes.length - 1; i >= 0; --i) {
            result = add(multiply(result, data), bytes[i]);
        }
        return result;
    }

    public static byte recover(final byte[][] bytes) {
        final byte x = 0;
        byte y = 0;
        for (int i = 0; (i < bytes.length); ++i) {
            final byte x1 = bytes[i][0];
            final byte y1 = bytes[i][1];
            byte i1 = 1;
            for (int j = 0; (j < bytes.length); ++j) {
                final byte x2 = bytes[j][0];
                if (i != j) {
                    i1 = multiply(i1, divide(subtract(x, x2), subtract(x1, x2)));
                }
            }
            y = add(y, multiply(i1, y1));
        }
        return y;
    }

    // additional info:
    //   https://research.swtch.com/field
    //   https://en.wikiversity.org/wiki/Reed%E2%80%93Solomon_codes_for_coders#Addition_and_Subtraction

    private static byte add(final byte x, final byte y) {
        return (byte) (x ^ y);
    }

    private static byte subtract(final byte x, final byte y) {
        return (byte) (x ^ y);
    }

    private static byte multiply(final byte x, final byte y) {
        final byte product;
        if ((x == 0) || (y == 0)) {
            product = 0;
        } else {
            product = EXP[toUInt(LOG[toUInt(x)]) + toUInt(LOG[toUInt(y)])];
        }
        return product;
    }

    private static byte divide(final byte x, final byte y) {
        return multiply(x, EXP[(LOG.length - 1) - toUInt(LOG[toUInt(y)])]);
    }

    private static int toUInt(final byte x) {
        return ((int) x) & LOW_BYTE_MASK;
    }

    private static final int LOW_BYTE_MASK = (1 << Byte.SIZE) - 1;
}
