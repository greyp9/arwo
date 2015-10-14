package io.github.greyp9.arwo.core.xsd.define;

import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataTypeRestrictions;

import java.util.ArrayList;
import java.util.Collection;

public class DataTypeRestrictionsFactory {
    private final XsdAtom restriction;

    public DataTypeRestrictionsFactory(final XsdAtom restriction) {
        this.restriction = restriction;
    }

    public final DataTypeRestrictions create() {
        Collection<String> enumValues = new ArrayList<String>();
        Collection<String> patterns = new ArrayList<String>();
        Long minInclusive = null;
        Long maxInclusive = null;
        Integer minLength = null;
        Integer maxLength = null;
        for (final XsdAtom atom : restriction.getChildren()) {
            final String name = atom.getElement().getLocalName();
            if (XsdU.ENUMERATION.equals(name)) {
                enumValues = updateEnumeration(atom, enumValues);
            } else if (XsdU.PATTERN.equals(name)) {
                patterns = updatePattern(atom, patterns);
            } else if (XsdU.MIN_INCLUSIVE.equals(name)) {
                minInclusive = updateMinInclusive(atom, minInclusive);
            } else if (XsdU.MAX_INCLUSIVE.equals(name)) {
                maxInclusive = updateMaxInclusive(atom, maxInclusive);
            } else if (XsdU.MIN_LENGTH.equals(name)) {
                minLength = updateMinLength(atom, minLength);
            } else if (XsdU.MAX_LENGTH.equals(name)) {
                maxLength = updateMaxLength(atom, maxLength);
            }
        }
        return new DataTypeRestrictions(enumValues, patterns, minInclusive, maxInclusive, minLength, maxLength);
    }

    private Collection<String> updateEnumeration(final XsdAtom atom, final Collection<String> enumValues) {
        final String value = atom.getElement().getAttribute(XsdU.VALUE);
        enumValues.add(value);
        return enumValues;
    }

    private Collection<String> updatePattern(final XsdAtom atom, final Collection<String> patterns) {
        final String value = atom.getElement().getAttribute(XsdU.VALUE);
        patterns.add(value);
        return patterns;
    }

    private Long updateMinInclusive(final XsdAtom atom, final Long minInclusive) {
        final String value = atom.getElement().getAttribute(XsdU.VALUE);
        final Long minInclusiveIt = NumberU.toLong(value, Integer.MIN_VALUE);
        return ((minInclusive == null) ? minInclusiveIt : Math.min(minInclusive, minInclusiveIt));
    }

    private Long updateMaxInclusive(final XsdAtom atom, final Long maxInclusive) {
        final String value = atom.getElement().getAttribute(XsdU.VALUE);
        final Long maxInclusiveIt = NumberU.toLong(value, Integer.MAX_VALUE);
        return ((maxInclusive == null) ? maxInclusiveIt : Math.max(maxInclusive, maxInclusiveIt));
    }

    private Integer updateMinLength(final XsdAtom atom, final Integer minLength) {
        final String value = atom.getElement().getAttribute(XsdU.VALUE);
        final Integer minLengthIt = NumberU.toInt(value, 0);
        return ((minLength == null) ? minLengthIt : Math.min(minLength, minLengthIt));
    }

    private Integer updateMaxLength(final XsdAtom atom, final Integer maxLength) {
        final String value = atom.getElement().getAttribute(XsdU.VALUE);
        final Integer maxLengthIt = NumberU.toInt(value, Integer.MAX_VALUE);
        return ((maxLength == null) ? maxLengthIt : Math.max(maxLength, maxLengthIt));
    }
}
