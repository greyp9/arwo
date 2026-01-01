package io.github.greyp9.arwo.app.core.view.filter;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class FilterMenuFactory {
    private final Xed xed;
    private final String xpath;
    private final String typeInstanceName;
    private final String filterType;

    public FilterMenuFactory(final Xed xed,
                             final String xpath,
                             final String typeInstanceName,
                             final String filterType) {
        this.xed = xed;
        this.xpath = xpath;
        this.typeInstanceName = typeInstanceName;
        this.filterType = filterType;
    }

    public final MenuItem create(final String id, final String type, final String object2) {
        final Collection<NameTypeValue> nameTypeValues = getFilters(
                App.Settings.NAME, App.Settings.CONTEXT, App.Settings.EXPRESSION);
        final Collection<NameTypeValue> nameTypeValuesForContext = nameTypeValues.stream()
                .filter(ntv -> filterType.equals(ntv.getType()))
                .collect(Collectors.toList());
        final List<MenuItem> menuItems = new ArrayList<>();
        for (NameTypeValue ntv : nameTypeValuesForContext) {
            menuItems.add(new MenuItem(ntv.getName(), App.Target.VIEW_STATE, ViewState.Action.APPLY_FILTER,
                    filterType, ntv.getValueS()));
        }

        return new MenuItem("Filters", App.Target.USER_STATE, App.Action.MENU2,
                PathU.toPath(Http.Token.SLASH, id, type), object2, menuItems);
    }

    private Collection<NameTypeValue> getFilters(final String nameKey, final String typeKey, final String valueKey) {
        final Collection<NameTypeValue> nameTypeValues = new ArrayList<>();
        final XedNav nav = new XedNav(xed);
        try {
            final XedCursor cursorFilters = nav.findX(xpath);
            final TypeInstance typeInstanceFilter = cursorFilters.getChildInstance(typeInstanceName);
            final Collection<Element> childrenFilter = cursorFilters.getChildren(typeInstanceFilter);
            for (final Element childFilter : childrenFilter) {
                final XedCursor cursorFilter = nav.find(childFilter, cursorFilters);
                final String name = cursorFilter.getValue(cursorFilter.getChildInstance(nameKey));
                final String type = cursorFilter.getValue(cursorFilter.getChildInstance(typeKey));
                final String value = cursorFilter.getValue(cursorFilter.getChildInstance(valueKey));
                if (!Value.isEmpty(name)) {
                    nameTypeValues.add(new NameTypeValue(name, type, value));
                }
            }
        } catch (IOException e) {
            Logger.getLogger(getClass().getName()).severe(e.getMessage());
        }
        return nameTypeValues;
    }
}
