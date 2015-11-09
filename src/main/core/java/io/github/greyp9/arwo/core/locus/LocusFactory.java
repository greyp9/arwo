package io.github.greyp9.arwo.core.locus;

import io.github.greyp9.arwo.core.date.DateX;

import java.util.Locale;

public class LocusFactory {

    public final Locus create(final String localeID, final DateX dateX) {
        final Locale locale = ((localeID == null) ? Locale.getDefault() : new Locale(localeID));
        return new Locus(locale, dateX);
    }
}
