package io.github.greyp9.arwo.core.text;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.Value;

public final class TextU {

    private TextU() {
    }

    public static String wrapBracket(final String value) {
        return Value.wrap(App.Token.BRACKET_OPEN, App.Token.BRACKET_CLOSE, value);
    }
}
