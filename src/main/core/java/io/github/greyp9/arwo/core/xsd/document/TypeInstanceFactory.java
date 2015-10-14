package io.github.greyp9.arwo.core.xsd.document;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.DataTypeRestrictions;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitionsFactory;

import javax.xml.namespace.QName;
import java.util.Date;
import java.util.UUID;

public class TypeInstanceFactory {  // NOPMD

    public final String getDefaultValue(final TypeInstance typeInstance) {
        final String defaultValue = typeInstance.getDefault();
        return (defaultValue == null) ? getDefaultValue(typeInstance.getDataType()) : defaultValue;
    }

    public final String getDefaultValue(final DataType dataType) {
        return getDefaultValue(dataType, dataType);
    }

    private String getDefaultValue(final DataType parentType, final DataType type) {  // NOPMD
        String value = null;
        final QName qname = type.getQName();
        if (TypeDefinitionsFactory.Const.ANY_URI.equals(qname)) {
            value = Const.STRING_DEFAULT;
        } else if (TypeDefinitionsFactory.Const.BASE_64_BINARY.equals(qname)) {
            value = Const.STRING_DEFAULT;
        } else if (TypeDefinitionsFactory.Const.BOOLEAN.equals(qname)) {
            value = Const.BOOLEAN_DEFAULT;
        } else if (TypeDefinitionsFactory.Const.DATE.equals(qname)) {
            value = DateU.toString(Const.DATE_DEFAULT, DateU.Const.TZ_GMT.getID(), XsdDateU.Const.DATE);
        } else if (TypeDefinitionsFactory.Const.DATE_TIME.equals(qname)) {
            value = DateU.toString(Const.DATE_DEFAULT, DateU.Const.TZ_GMT.getID(), XsdDateU.Const.DATETIME);
        } else if (TypeDefinitionsFactory.Const.DECIMAL.equals(qname)) {
            value = getDefaultValueRestricted(Const.LONG_DEFAULT, parentType);
        } else if (TypeDefinitionsFactory.Const.DURATION.equals(qname)) {
            value = Const.DURATION_DEFAULT;
        } else if (TypeDefinitionsFactory.Const.ID.equals(qname)) {
            value = UUID.randomUUID().toString();
        } else if (TypeDefinitionsFactory.Const.INT.equals(qname)) {
            value = getDefaultValueRestricted((Long) null, parentType); // restrict/enum logic needs start from null
        } else if (TypeDefinitionsFactory.Const.LONG.equals(qname)) {
            value = getDefaultValueRestricted(Const.LONG_DEFAULT, parentType);
        } else if (TypeDefinitionsFactory.Const.INT.equals(qname)) {
            value = getDefaultValueRestricted(Const.LONG_DEFAULT, parentType);
        } else if (TypeDefinitionsFactory.Const.INTEGER.equals(qname)) {
            value = getDefaultValueRestricted(Const.LONG_DEFAULT, parentType);
        } else if (TypeDefinitionsFactory.Const.NORMALIZED_STRING.equals(qname)) {
            value = getDefaultValueRestricted((String) null, parentType); // restrict/enum logic needs start from null
        } else if (TypeDefinitionsFactory.Const.STRING.equals(qname)) {
            value = getDefaultValueRestricted((String) null, parentType); // restrict/enum logic needs start from null
        } else if (TypeDefinitionsFactory.Const.TIME.equals(qname)) {
            value = DateU.toString(Const.DATE_DEFAULT, DateU.Const.TZ_GMT.getID(), XsdDateU.Const.TIME);
        } else if (TypeDefinitionsFactory.Const.UNSIGNED_BYTE.equals(qname)) {
            value = getDefaultValueRestricted(Const.LONG_DEFAULT, parentType);
        } else if (TypeDefinitionsFactory.Const.UNSIGNED_INT.equals(qname)) {
            value = getDefaultValueRestricted(Const.LONG_DEFAULT, parentType);
        } else if (TypeDefinitionsFactory.Const.UNSIGNED_LONG.equals(qname)) {
            value = getDefaultValueRestricted(Const.LONG_DEFAULT, parentType);
        } else if (TypeDefinitionsFactory.Const.UNSIGNED_SHORT.equals(qname)) {
            value = getDefaultValueRestricted(Const.LONG_DEFAULT, parentType);
        } else if (type.getBaseType() != null) {
            value = getDefaultValue(parentType, type.getBaseType());
        }
        return value;
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private String getDefaultValueRestricted(final Long valueIn, final DataType type) {
        Long value = valueIn;
        final DataTypeRestrictions restrictions = type.getRestrictions();
        final Long minInclusive = restrictions.getMinInclusive();
        final Long maxInclusive = restrictions.getMaxInclusive();
        // bounded value (min, max)
        if (minInclusive != null) {
            value = ((value == null) ? minInclusive : Math.max(value, minInclusive));
        }
        if (maxInclusive != null) {
            value = ((value == null) ? maxInclusive : Math.min(value, maxInclusive));
        }
        // enumerated value (first)
        if ((value == null) && (!restrictions.getEnumValues().isEmpty())) {
            value = Long.valueOf(restrictions.getEnumValues().iterator().next());
        }
        return Long.toString((value == null) ? 0L : value);
    }

    private String getDefaultValueRestricted(final String valueIn, final DataType type) {
        String value = valueIn;
        // enumerated value (first)
        final DataTypeRestrictions restrictions = type.getRestrictions();
        if ((value == null) && (!restrictions.getEnumValues().isEmpty())) {
            value = restrictions.getEnumValues().iterator().next();
        }
        // bounded data length
        final Integer minLength = restrictions.getMinLength();
        final Integer maxLength = restrictions.getMaxLength();
        if (minLength != null) {
            value = StringU.appendToLength(minLength, 'X', value);
        }
        if (maxLength != null) {
            value = StringU.truncateToLength(maxLength, value);
        }
        return ((value == null) ? "" : value);
    }

    public static class Const {
        private static final Date DATE_DEFAULT = new Date(0L);
        private static final Long LONG_DEFAULT = 0L;
        private static final String BOOLEAN_DEFAULT = Boolean.FALSE.toString();
        private static final String DURATION_DEFAULT = "PT0S";
        private static final String STRING_DEFAULT = "";
    }
}
