package io.github.greyp9.arwo.core.xsd.data;

import java.util.ArrayList;
import java.util.Collection;

public class DataTypeRestrictions {
    private final Collection<String> enumValues;
    private final Collection<String> patterns;

    private final Long minInclusive;
    private final Long maxInclusive;

    private final Integer minLength;
    private final Integer maxLength;

    public DataTypeRestrictions() {
        this(null, null, null, null, null, null);
    }

    public final Collection<String> getEnumValues() {
        return enumValues;
    }

    public final Collection<String> getPatterns() {
        return patterns;
    }

    public final Long getMinInclusive() {
        return minInclusive;
    }

    public final Long getMaxInclusive() {
        return maxInclusive;
    }

    public final Integer getMinLength() {
        return minLength;
    }

    public final Integer getMaxLength() {
        return maxLength;
    }

    public DataTypeRestrictions(
            final Collection<String> enumValues, final Collection<String> patterns,
            final Long minInclusive, final Long maxInclusive,
            final Integer minLength, final Integer maxLength) {
        this.enumValues = ((enumValues == null) ? new ArrayList<String>() : enumValues);
        this.patterns = ((patterns == null) ? new ArrayList<String>() : patterns);
        this.minInclusive = minInclusive;
        this.maxInclusive = maxInclusive;
        this.minLength = minLength;
        this.maxLength = maxLength;
    }
}
