package io.github.greyp9.arwo.core.text.render;

import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.text.line.LineU;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

public final class TextRenderer {
    private final int characterCount;
    private final Collection<String> lines;

    public TextRenderer(final String text) throws IOException {
        this.characterCount = ((text == null) ? 0 : text.length());
        this.lines = LineU.toLines(text);
    }

    public int getCharacterCount() {
        return characterCount;
    }

    public int getLineCount() {
        return lines.size();
    }

    public String render(final int maxLines) {
        final ArrayList<String> linesRender = new ArrayList<String>(lines);
        while (linesRender.size() > maxLines) {
            linesRender.remove(0);
        }
        return LineU.toText(linesRender, SystemU.eol());
    }

    public static class Const {
        public static final int SCROLLBACK_LINES = 3000;
    }
}
