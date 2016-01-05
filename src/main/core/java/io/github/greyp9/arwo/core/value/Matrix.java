package io.github.greyp9.arwo.core.value;

import io.github.greyp9.arwo.core.lang.SystemU;

public class Matrix {
    private final int rows;
    private final int columns;
    private final Object[][] values;

    public Matrix(final int rows, final int columns) {
        this.rows = rows;
        this.columns = columns;
        this.values = new Object[rows][columns];
    }

    public final void set(final int row, final int column, final Object value) {
        values[row][column] = value;
    }

    public final String render(final String spacer) {
        final int[] widths = getWidths();
        final StringBuilder buffer = new StringBuilder();
        for (int row = 0; (row < rows); ++row) {
            buffer.append(renderRow(spacer, row, widths));
        }
        return buffer.toString();
    }

    @SuppressWarnings("PMD.UseVarargs")
    private String renderRow(final String spacer, final int row, final int[] widths) {
        final StringBuilder buffer = new StringBuilder();
        for (int column = 0; (column < columns); ++column) {
            final String format = "%-" + widths[column] + "s";  // i18n
            buffer.append((column == 0) ? "" : spacer);
            buffer.append(String.format(format, values[row][column]));
        }
        buffer.append(SystemU.eol());
        return buffer.toString();
    }

    private int[] getWidths() {
        final int[] widths = new int[columns];
        for (int row = 0; (row < rows); ++row) {
            for (int column = 0; (column < columns); ++column) {
                final Object value = values[row][column];
                final int widthCell = (value == null) ? 1 : value.toString().length();
                widths[column] = Math.max(widths[column], widthCell);
            }
        }
        return widths;
    }
}
