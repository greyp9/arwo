package io.github.greyp9.arwo.core.ff.test;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.ff.FiniteField;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import org.junit.Assert;
import org.junit.Test;

import java.util.logging.Logger;

public class FiniteFieldGeneratorTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testLookupTables() {
        Assert.assertEquals(
                "07ec21ce3f045f5d9a2121cf58798c94510122ad81272411a50e2918382ac428",
                FiniteField.sha256Exp());
        Assert.assertEquals(
                "9bc7b6f7f6423e975f3a7d66c47d9868957ee5a3fa2e94c25763220e692d2c69",
                FiniteField.sha256Log());
    }

    /**
     * Calculate GF(256).
     */
    @Test
    public void testGenerateLookupTables() {
        // https://en.wikiversity.org/wiki/Reed%E2%80%93Solomon_codes_for_coders#Finite_field_arithmetic
        final int sizeFiniteFieldLog = FINITE_FIELD_SIZE;
        final int sizeFiniteFieldExponent = sizeFiniteFieldLog * 2;

        final byte[] bytesFiniteFieldExponent = new byte[sizeFiniteFieldExponent];
        bytesFiniteFieldExponent[0] = 1;
        for (int i = 1; (i < (sizeFiniteFieldExponent - 2)); ++i) {
            bytesFiniteFieldExponent[i] = multiply(bytesFiniteFieldExponent[i - 1], AES_GENERATOR);
        }
        logger.finest(HexCodec.encode(bytesFiniteFieldExponent));
        Assert.assertEquals(
                "07ec21ce3f045f5d9a2121cf58798c94510122ad81272411a50e2918382ac428",
                HexCodec.encode(HashU.sha256(bytesFiniteFieldExponent)));

        final byte[] bytesFiniteFieldLog = new byte[sizeFiniteFieldLog];
        for (int i = 0; (i < sizeFiniteFieldLog); ++i) {
            bytesFiniteFieldLog[bytesFiniteFieldExponent[i] & LOW_BYTE_MASK] = (byte) i;
        }
        logger.finest(HexCodec.encode(bytesFiniteFieldLog));
        Assert.assertEquals(
                "f1930ba3ffbadd49b15e5720a34f4eee56d5ad634dfd5aee683f999bc0e2b970",
                HexCodec.encode(HashU.sha256(bytesFiniteFieldLog)));
    }

    /**
     * Adapted from C language example <a href="https://en.wikipedia.org/wiki/Finite_field_arithmetic">here.</a>
     */
    private byte multiply(final int a, final int b) {
        int aa = a;
        int bb = b;
        int p = 0;
        while ((aa != 0) && (bb != 0)) {
            if ((bb & 0x01) != 0) {  // if isOdd
                p ^= aa;
            }
            if ((aa & HIGH_BIT) != 0) {  // if highBit
                aa = (aa << 1) ^ AES_FIELD_POLYNOMIAL;
            } else {
                aa <<= 1;
            }
            bb >>= 1;
        }
        return toByte(p);
    }

    private static final int FINITE_FIELD_SIZE = 256; // Math.pow(2, 8)
    private static final int AES_GENERATOR = 0x03;
    private static final int AES_FIELD_POLYNOMIAL = 0x11b;  // x^8 + x^4 + x^3 + x + 1 (0001_0001_1011)
    private static final int LOW_BYTE_MASK = (1 << Byte.SIZE) - 1;
    private static final int HIGH_BIT = 0x80;

    private static byte toByte(final int i) {
        return (byte) (i & LOW_BYTE_MASK);
    }
}
