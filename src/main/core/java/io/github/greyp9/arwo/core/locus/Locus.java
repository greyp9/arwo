package io.github.greyp9.arwo.core.locus;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.lang.NumberU;

import java.sql.Types;
import java.util.Date;
import java.util.Locale;

public class Locus {
    private final Locale locale;
    private final DateX dateX;

    public final Locale getLocale() {
        return locale;
    }

    public final DateX getDateX() {
        return dateX;
    }

    public Locus(final Locale locale, final DateX dateX) {
        this.locale = locale;
        this.dateX = dateX;
    }

    public final String toString(final Date value) {
        return dateX.toString(value);
    }

    public final String toString(final Long value) {
        return ((value == null) ? null : String.format("%,d", value));  // i18n
    }

    public final String toString(final Integer value) {
        return ((value == null) ? null : String.format("%,d", value));  // i18n
    }

    public final String toString(final Object value) {
        String valueString;
        if (value == null) {
            valueString = null;
        } else if (value instanceof Date) {
            valueString = toString((Date) value);
        } else if (value instanceof Long) {
            valueString = toString((Long) value);
        } else if (value instanceof Integer) {
            valueString = toString((Integer) value);
        } else {
            valueString = value.toString();
        }
        return valueString;
    }

    public final Date toDate(final String value) {
        return dateX.toDate(value);
    }

    public final Long toLong(final String value) {
        return NumberU.toLong(value, 0L);
    }

    public final Integer toInt(final String value) {
        return NumberU.toInt(value, 0);
    }

    public final Object toValue(final int type, final String value) {
        Object valueTyped = value;
        if (type == Types.TIMESTAMP) {
            valueTyped = toDate(value);
        } else if (type == Types.BIGINT) {
            valueTyped = toLong(value);
        } else if (type == Types.INTEGER) {
            valueTyped = toInt(value);
        }
        return valueTyped;
    }
}
