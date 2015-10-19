package io.github.greyp9.arwo.core.xed.model;

import java.util.Locale;

public class XedFactory {

    public final Xed update(final Xed xed, final Locale locale) {
        return new Xed(xed.getDocument(), xed.getXsdTypes(), locale);
    }
}
