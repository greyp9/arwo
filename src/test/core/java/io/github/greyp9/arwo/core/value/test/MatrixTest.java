package io.github.greyp9.arwo.core.value.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.value.Matrix;
import junit.framework.TestCase;

import java.util.logging.Logger;

public class MatrixTest extends TestCase {
    Logger logger = Logger.getLogger(getClass().getName());

    public void testMatrix() throws Exception {
        final int rows = 3, columns = 3;
        final Matrix matrix = new Matrix(rows, columns);
        for (int row = 0; (row < rows); ++row) {
            for (int column = 0; (column < columns); ++column) {
                matrix.set(row, column, (row * column));
            }
        }
        final String render = matrix.render(" | ");
        logger.info("3 x 3 matrix\n" + render);
        logger.info(CRCU.crc32String(UTF8Codec.toBytes(render)) + "/" + render.length());
    }

    public void testMatrixX() throws Exception {
        final int rows = 3, columns = 3;
        final Matrix matrix = new Matrix(rows, columns);
        for (int row = 0; (row < rows); ++row) {
            for (int column = 0; (column < columns); ++column) {
                matrix.set(row, column, (row * column));
            }
        }
        final String render = matrix.render(" | ");
        logger.info("3 x 3 matrix\n" + render);
        logger.info(CRCU.crc32String(UTF8Codec.toBytes(render)) + "/" + render.length());
    }

    public void testMatrixXColumnHeaders() throws Exception {
        final int rows = 3, columns = 3;
        final Matrix matrix = new Matrix(rows, columns);
        for (int row = 0; (row < rows); ++row) {
            matrix.setRowLabel(row, Integer.toString(row * 11));
            for (int column = 0; (column < columns); ++column) {
                matrix.setColumnLabel(column, Integer.toString(column * 11));
                matrix.set(row, column, (row * column));
            }
        }
        final String render = matrix.render(" | ");
        logger.info("3 x 3 matrix (row labels, column labels)\n" + render);
        logger.info(CRCU.crc32String(UTF8Codec.toBytes(render)) + "/" + render.length());
    }
}
