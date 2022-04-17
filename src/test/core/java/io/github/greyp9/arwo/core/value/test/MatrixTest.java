package io.github.greyp9.arwo.core.value.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.value.Matrix;
import org.junit.Test;

import java.util.logging.Logger;

public class MatrixTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testMatrix() throws Exception {
        final int rows = 3;
        final int columns = 3;
        final Matrix matrix = new Matrix(rows, columns);
        for (int row = 0; (row < rows); ++row) {
            for (int column = 0; (column < columns); ++column) {
                matrix.set(row, column, (row * column));
            }
        }
        final String render = matrix.render(" | ");
        logger.finest("3 x 3 matrix\n" + render);
        logger.finest(CRCU.crc32String(UTF8Codec.toBytes(render)) + "/" + render.length());
    }

    @Test
    public void testMatrixX() throws Exception {
        final int rows = 3;
        final int columns = 3;
        final Matrix matrix = new Matrix(rows, columns);
        for (int row = 0; (row < rows); ++row) {
            for (int column = 0; (column < columns); ++column) {
                matrix.set(row, column, (row * column));
            }
        }
        final String render = matrix.render(" | ");
        logger.finest("3 x 3 matrix\n" + render);
        logger.finest(CRCU.crc32String(UTF8Codec.toBytes(render)) + "/" + render.length());
    }

    @Test
    public void testMatrixXColumnHeaders() throws Exception {
        final int rows = 3;
        final int columns = 3;
        final int rowMultiple = 11;
        final Matrix matrix = new Matrix(rows, columns);
        for (int row = 0; (row < rows); ++row) {
            matrix.setRowLabel(row, Integer.toString(row * rowMultiple));
            for (int column = 0; (column < columns); ++column) {
                matrix.setColumnLabel(column, Integer.toString(column * rowMultiple));
                matrix.set(row, column, (row * column));
            }
        }
        final String render = matrix.render(" | ");
        logger.finest("3 x 3 matrix (row labels, column labels)\n" + render);
        logger.finest(CRCU.crc32String(UTF8Codec.toBytes(render)) + "/" + render.length());
    }
}
