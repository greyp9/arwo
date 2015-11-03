package io.github.greyp9.arwo.core.xed.table;

import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.xsd.core.XsdTypeU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;

import javax.xml.namespace.QName;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class XedMetaDataFactory {

    public final RowSetMetaData create(final TypeInstance typeInstance, final boolean rowLinks) {
        final ArrayList<ColumnMetaData> columns = new ArrayList<ColumnMetaData>();
        if (rowLinks) {
            columns.add(new ColumnMetaData("...", Types.VARCHAR, false));
        }
        // populate column data
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(typeInstance).getTableInstances();
        for (final TypeInstance typeInstanceC : typeInstances) {
            iterateMetaDataColumn(columns, typeInstanceC);
        }
        return new RowSetMetaData(typeInstance.getID(), columns.toArray(new ColumnMetaData[columns.size()]));
    }

    private void iterateMetaDataColumn(final List<ColumnMetaData> columns, final TypeInstance typeInstanceC) {
        // add table column
        final String name = typeInstanceC.getName();
        final boolean identity = typeInstanceC.isIdentity();
        final DataType dataType = typeInstanceC.getDataType();
        final int type = toSqlType(dataType);
        columns.add(new ColumnMetaData(name, type, identity));
    }

    private static int toSqlType(final DataType dataType) {
        int type = Types.VARCHAR;
        final QName qname = ((dataType == null) ? null : dataType.getQName());
        if (XsdTypeU.Const.DATE_TIME.equals(qname)) {
            type = Types.TIMESTAMP;
        } else if (XsdTypeU.Const.UNSIGNED_INT.equals(qname)) {
            type = Types.INTEGER;
        } else if (XsdTypeU.Const.BOOLEAN.equals(qname)) {
            type = Types.BOOLEAN;
        }
        return type;
    }
}
