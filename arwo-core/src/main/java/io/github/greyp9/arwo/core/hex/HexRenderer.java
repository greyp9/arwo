package io.github.greyp9.arwo.core.hex;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.lang.StringU;

public class HexRenderer {
    private final int lineWidth;

    public HexRenderer(final int lineWidth) {
        this.lineWidth = lineWidth;
    }

    public final String render(final byte[] bytes, final int start, final int finish) {
        final StringBuilder buffer = new StringBuilder();
        for (int i = start; ((i < finish) && (i < bytes.length)); i += lineWidth) {
            final String hex = renderHex(bytes, i);
            final String printable = renderPrintable(bytes, i);
            buffer.append(String.format("%8d %8h  |  %s  |  %s%n", i, i, hex, printable));
        }
        return buffer.toString();
    }

    private String renderHex(final byte[] bytes, final int position) {
        final StringBuilder buffer = new StringBuilder();
        for (int i = position; (i < (position + lineWidth)); i += Const.WORD) {
            buffer.append((buffer.length() == 0) ? "" : Const.INTER_WORD);
            for (int j = 0; (j < Const.WORD); ++j) {
                final boolean inRange = ((i + j) < bytes.length);
                buffer.append((buffer.length() == 0) ? "" : Const.INTRA_WORD);
                buffer.append(inRange ? renderHex(bytes[i + j]) : Const.INTER_WORD);
            }
        }
        return buffer.toString();
    }

    private String renderHex(final byte b) {
        return String.format("%02x", (b & Const.ONE_BYTE_MASK));  // i18n internal
    }

    private String renderPrintable(final byte[] bytes, final int position) {
        final StringBuilder buffer = new StringBuilder();
        for (int i = position; (i < (position + lineWidth)); i += Const.WORD) {
            buffer.append((buffer.length() == 0) ? "" : Const.INTRA_WORD);
            for (int j = 0; (j < Const.WORD); ++j) {
                final boolean inRange = ((i + j) < bytes.length);
                buffer.append(inRange ? renderPrintable(bytes[i + j]) : Const.INTER_WORD);
            }
        }
        return buffer.toString();
    }

    private String renderPrintable(final byte b) {
        final boolean isPrintable = ((b >= Const.PRINTABLE_LOW) && (b <= Const.PRINTABLE_HIGH));
        return isPrintable ? Character.toString((char) b) : Http.Token.DOT;
    }

    private static class Const {
        private static final int WORD = 8;

        private static final byte PRINTABLE_LOW = 0x20;
        private static final byte PRINTABLE_HIGH = 0x7e;
        private static final int ONE_BYTE_MASK = 0xff;

        private static final String INTRA_WORD = StringU.create(1, Html.SPACE);
        private static final String INTER_WORD = StringU.create(2, Html.SPACE);
    }
}
