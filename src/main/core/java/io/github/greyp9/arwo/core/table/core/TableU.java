package io.github.greyp9.arwo.core.table.core;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.value.Value;

import java.sql.Types;
import java.text.MessageFormat;

public final class TableU {

    private TableU() {
    }

    public static String toCSS(final int type, final Object value) {
        final String align;
        if (value instanceof Number) {
            align = App.CSS.NUMBER;
        } else if (type == Types.BIGINT) {
            align = App.CSS.NUMBER;
        } else if (type == Types.DATALINK) {
            align = App.CSS.LINK;
        } else if (type == Types.VARCHAR) {
            align = App.CSS.TEXT;
        } else if (type == Types.TIMESTAMP) {
            align = App.CSS.TEXT;
        } else if (type == Types.BOOLEAN) {
            align = App.CSS.TEXT;
        } else {
            align = null;
        }
        return align;
    }

    public static void addFooterStandard(final Table table, final Bundle bundle) {
        final String pattern = bundle.getString(Table.Const.FOOTER_SIZE);
        final String label = MessageFormat.format(pattern, table.getRows());
        table.getProperties().setProperty(Table.Const.FOOTER_C, label);
    }

    public static String getKey(final String tableID, final String columnName) {
        return Value.join(".", tableID, columnName);
    }
}
