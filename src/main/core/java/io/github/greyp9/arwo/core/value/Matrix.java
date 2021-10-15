package io.github.greyp9.arwo.core.value;

import io.github.greyp9.arwo.core.lang.SystemU;

public class Matrix {
    private final int rows;
    private final int columns;
    private final String[] rowLabels;
    private final String[] columnLabels;
    private final Object[][] values;

    public Matrix(final int rows, final int columns) {
        this.rows = rows;
        this.columns = columns;
        this.rowLabels = new String[rows];
        this.columnLabels = new String[columns];
        this.values = new Object[rows][columns];
    }

    public int getRows() {
        return rows;
    }

    public int getColumns() {
        return columns;
    }

    public final void setRowLabel(final int row, final String value) {
        rowLabels[row] = value;
    }

    public final void setColumnLabel(final int column, final String value) {
        columnLabels[column] = value;
    }

    public final void set(final int row, final int column, final Object value) {
        values[row][column] = value;
    }

    public final String render(final String spacer) {
        return new Renderer(this, spacer, Integer.MAX_VALUE).render();
    }

    public final String render(final String spacer, int linesPerParagraph) {
        return new Renderer(this, spacer, linesPerParagraph).render();
    }

    private static class Renderer {
        private final Matrix matrix;
        private final String spacer;
        private final int linesPerParagraph;
        private final int widthRCL;
        private final int widthCRL;
        private final int[] widths;

        public Renderer(Matrix matrix, String spacer, int linesPerParagraph) {
            this.matrix = matrix;
            this.spacer = spacer;
            this.linesPerParagraph = linesPerParagraph;
            this.widthRCL = getWidthRowColumnHeaders();
            this.widthCRL = getWidthColumnRowHeaders();
            this.widths = getWidths();
        }

        public final String render() {
            final StringBuilder buffer = new StringBuilder();
            if (widthRCL > 0) {
                buffer.append(renderRowColumnHeaders());
            }
            int lineInParagraph = 0;
            for (int row = 0; (row < matrix.getRows()); ++row) {
                buffer.append(renderRow(row));
                // paragraph divider (for readability)
                if (++lineInParagraph % linesPerParagraph == 0) {
                    buffer.append(SystemU.eol());
                }
            }
            return buffer.toString();
        }

        private String renderRowColumnHeaders() {
            final StringBuilder buffer = new StringBuilder();
            // row label column header label
            if (widthCRL > 0) {
                buffer.append(renderCell(widthCRL, ""));
            }
            // column header labels
            for (int column = 0; (column < matrix.getColumns()); ++column) {
                if ((widthCRL > 0) || (column > 0)) {
                    buffer.append(spacer);
                }
                buffer.append(renderCell(widths[column], matrix.columnLabels[column]));
            }
            buffer.append(SystemU.eol());
            return buffer.toString();
        }

        private String renderRow(final int row) {
            final StringBuilder buffer = new StringBuilder();
            // column row label
            if (widthCRL > 0) {
                buffer.append(renderCell(widthCRL, matrix.rowLabels[row]));
            }
            // row data columns
            for (int column = 0; (column < matrix.getColumns()); ++column) {
                if ((widthCRL > 0) || (column > 0)) {
                    buffer.append(spacer);
                }
                buffer.append(renderCell(widths[column], matrix.values[row][column]));
            }
            buffer.append(SystemU.eol());
            return buffer.toString();
        }

        private static String renderCell(final int width, final Object value) {
            final String widthFormat = (width == 0) ? "" : ("" + width);
            final String format = ((value instanceof Number) || (value == null))
                    ? ("%" + widthFormat + "s") : ("%-" + widthFormat + "s");
            return String.format(format, Value.defaultOnNull(value, ""));
        }

        private int getWidthRowColumnHeaders() {
            int width = 0;
            for (int column = 0; (column < matrix.getColumns()); ++column) {
                width += widthMax(0, matrix.columnLabels[column]);
            }
            return width;
        }

        private int getWidthColumnRowHeaders() {
            int width = 0;
            for (int row = 0; (row < matrix.getRows()); ++row) {
                width = widthMax(width, matrix.rowLabels[row]);
            }
            return width;
        }

        private int[] getWidths() {
            final int[] widths = new int[matrix.getColumns()];
            // initialize to width of column label
            for (int column = 0; (column < matrix.getColumns()); ++column) {
                widths[column] = widthMax(widths[column], matrix.columnLabels[column]);
            }
            // update to width of widest column
            for (int row = 0; (row < matrix.getRows()); ++row) {
                for (int column = 0; (column < matrix.getColumns()); ++column) {
                    widths[column] = widthMax(widths[column], matrix.values[row][column]);
                }
            }
            return widths;
        }

        private static int widthMax(final int widthMax, final Object value) {
            final int width = (value == null) ? 0 : value.toString().length();
            return Math.max(widthMax, width);
        }
    }
}
