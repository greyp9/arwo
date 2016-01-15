package io.github.greyp9.arwo.core.xsd.document;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xsd.core.XsdTypeU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.DataTypeRestrictions;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import javax.xml.namespace.QName;
import java.util.Date;
import java.util.UUID;

public class TypeInstanceFactory {  // NOPMD

    public final String getDefaultValue(final TypeInstance typeInstance) {
        final String defaultValue = typeInstance.getDefault();
        return (defaultValue == null) ? getDefaultValue(typeInstance.getDataType()) : defaultValue;
    }

    public final String getDefaultValue(final DataType dataType) {  // NOPMD
        final DataType baseType = dataType.getBaseType();
        String value = ((baseType == null) ? null : getDefaultValue(baseType));
        final QName qname = dataType.getQName();
        if (XsdTypeU.Const.ANY_URI.equals(qname)) {
            value = Const.STRING_DEFAULT;
        } else if (XsdTypeU.Const.BASE_64_BINARY.equals(qname)) {
            value = Const.STRING_DEFAULT;
        } else if (XsdTypeU.Const.BOOLEAN.equals(qname)) {
            value = Const.BOOLEAN_DEFAULT;
        } else if (XsdTypeU.Const.DATE.equals(qname)) {
            value = DateU.toString(Const.DATE_DEFAULT, DateU.Const.TZ_GMT.getID(), XsdDateU.Const.DATE);
        } else if (XsdTypeU.Const.DATE_TIME.equals(qname)) {
            value = DateU.toString(Const.DATE_DEFAULT, DateU.Const.TZ_GMT.getID(), XsdDateU.Const.DATETIME);
        } else if (XsdTypeU.Const.DECIMAL.equals(qname)) {
            value = Const.LONG_DEFAULT;
        } else if (XsdTypeU.Const.DURATION.equals(qname)) {
            value = Const.DURATION_DEFAULT;
        } else if (XsdTypeU.Const.ID.equals(qname)) {
            final String id = NumberU.toHex((int) UUID.randomUUID().getLeastSignificantBits());
            value = Value.join(XsdU.DASH, XsdTypeU.Const.ID.getLocalPart(), id);
        } else if (XsdTypeU.Const.INT.equals(qname)) {
            value = Const.LONG_DEFAULT;
        } else if (XsdTypeU.Const.LONG.equals(qname)) {
            value = Const.LONG_DEFAULT;
        } else if (XsdTypeU.Const.INT.equals(qname)) {
            value = Const.LONG_DEFAULT;
        } else if (XsdTypeU.Const.INTEGER.equals(qname)) {
            value = Const.LONG_DEFAULT;
        } else if (XsdTypeU.Const.NMTOKEN.equals(qname)) {
            final String id = NumberU.toHex((int) UUID.randomUUID().getLeastSignificantBits());
            value = Value.join(XsdU.DASH, XsdTypeU.Const.NMTOKEN.getLocalPart(), id);
        } else if (XsdTypeU.Const.NORMALIZED_STRING.equals(qname)) {
            value = Const.STRING_DEFAULT;
        } else if (XsdTypeU.Const.QNAME.equals(qname)) {
            value = Const.QNAME_DEFAULT;
        } else if (XsdTypeU.Const.STRING.equals(qname)) {
            value = Const.STRING_DEFAULT;
        } else if (XsdTypeU.Const.TIME.equals(qname)) {
            value = DateU.toString(Const.DATE_DEFAULT, DateU.Const.TZ_GMT.getID(), XsdDateU.Const.TIME);
        } else if (XsdTypeU.Const.TOKEN.equals(qname)) {
            value = Const.STRING_DEFAULT;
        } else if (XsdTypeU.Const.UNSIGNED_BYTE.equals(qname)) {
            value = Const.LONG_DEFAULT;
        } else if (XsdTypeU.Const.UNSIGNED_INT.equals(qname)) {
            value = Const.LONG_DEFAULT;
        } else if (XsdTypeU.Const.UNSIGNED_LONG.equals(qname)) {
            value = Const.LONG_DEFAULT;
        } else if (XsdTypeU.Const.UNSIGNED_SHORT.equals(qname)) {
            value = Const.LONG_DEFAULT;
        }
        return ((baseType == null) ? value : getDefaultValueRestricted(value, dataType));
    }

    private String getDefaultValueRestricted(final String value, final DataType dataType) {
        final DataTypeRestrictions restrictions = dataType.getRestrictions();
        String valueOut = getDefaultValueEnum(value, restrictions);
        valueOut = getDefaultValuePattern(valueOut, restrictions);
        valueOut = getDefaultValueMinLength(valueOut, restrictions);
        valueOut = getDefaultValueMaxLength(valueOut, restrictions);
        valueOut = getDefaultValueMinInclusive(valueOut, restrictions);
        return getDefaultValueMaxInclusive(valueOut, restrictions);
    }

    private String getDefaultValueMinInclusive(final String value, final DataTypeRestrictions restrictions) {
        String valueOut = value;
        final Long minInclusive = restrictions.getMinInclusive();
        if (minInclusive != null) {
            valueOut = Long.toString((value == null) ? minInclusive : Math.max(Long.parseLong(value), minInclusive));
        }
        return valueOut;
    }

    private String getDefaultValueMaxInclusive(final String value, final DataTypeRestrictions restrictions) {
        String valueOut = value;
        final Long maxInclusive = restrictions.getMaxInclusive();
        if (maxInclusive != null) {
            valueOut = Long.toString((value == null) ? maxInclusive : Math.min(Long.parseLong(value), maxInclusive));
        }
        return valueOut;
    }

    private String getDefaultValueEnum(final String value, final DataTypeRestrictions restrictions) {
        String valueOut = value;
        if (!restrictions.getEnumValues().isEmpty()) {
            valueOut = restrictions.getEnumValues().iterator().next();
        }
        return valueOut;
    }

    private String getDefaultValuePattern(final String value, final DataTypeRestrictions restrictions) {
        String valueOut = value;
        if (!restrictions.getPatterns().isEmpty()) {
            final String pattern = restrictions.getPatterns().iterator().next();
            if ("/.*".equals(pattern)) {
                valueOut = "/";  // i18n
            }
        }
        return valueOut;
    }

    private String getDefaultValueMinLength(final String value, final DataTypeRestrictions restrictions) {
        String valueOut = value;
        final Integer minLength = restrictions.getMinLength();
        if (minLength != null) {
            valueOut = StringU.appendToLength(minLength, 'X', value);
        }
        return valueOut;
    }

    private String getDefaultValueMaxLength(final String value, final DataTypeRestrictions restrictions) {
        String valueOut = value;
        final Integer maxLength = restrictions.getMaxLength();
        if (maxLength != null) {
            valueOut = StringU.truncateToLength(maxLength, value);
        }
        return valueOut;
    }

    public static class Const {
        private static final Date DATE_DEFAULT = new Date(0L);
        private static final String BOOLEAN_DEFAULT = Boolean.FALSE.toString();
        private static final String DURATION_DEFAULT = DurationU.Const.ZERO_SECONDS;
        private static final String LONG_DEFAULT = Long.toString(0L);
        private static final String QNAME_DEFAULT = "xmlns:qname";  // i18n
        private static final String STRING_DEFAULT = "";
    }
}
