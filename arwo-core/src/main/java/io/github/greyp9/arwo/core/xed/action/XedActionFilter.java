package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.table.filter.Filter;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xpath.XPather;

import java.io.IOException;
import java.util.Locale;

public class XedActionFilter extends XedAction {

    public XedActionFilter(final XedFactory factory, final Locale locale) throws IOException {
        super(App.Actions.QNAME_FILTER, factory, locale);
    }

    public final Filter getFilter(final NameTypeValues nameTypeValues) throws IOException {
        final Xed xed = super.update(nameTypeValues);
        // query xed instance for parameters
        final XPather xpather = xed.getXPather();
        final String columnString = xpather.getText("/action:filter/action:column");  // i18n xpath
        final String operatorString = xpather.getText("/action:filter/action:operator");  // i18n xpath
        final String valueString = xpather.getText("/action:filter/action:value");  // i18n xpath
        // null filter unless required data is present
        Filter filter = new Filter(-1, null, null, null);
        // attempt to load filter
        if (Value.notEmpty(columnString, operatorString)) {
            final Filter.Operator operator = Filter.toOperator(operatorString);
            if (operator != null) {
                final String value = Value.isEmpty(valueString) ? "" : valueString;
                filter = new Filter(-1, columnString, operator, value);
            }
        }
        return filter;
    }
}
