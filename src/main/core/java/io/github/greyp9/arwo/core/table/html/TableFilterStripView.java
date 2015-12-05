package io.github.greyp9.arwo.core.table.html;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Locale;

public class TableFilterStripView {
    private final TableContext context;

    public TableFilterStripView(final TableContext context) {
        this.context = context;
    }

    public final void addContentTo(final Element html) throws IOException {
        final boolean showFilterStrip = (context.getViewState().getFilterColumn() != null);
        if (showFilterStrip) {
            // filter model
            final Locale locale = context.getLocus().getLocale();
            final XedActionFilter actionFilter = new XedActionFilter(locale);
            final XedCursor cursorFilter = actionFilter.getCursor();
            final String filterColumn = context.getViewState().getFilterColumn();
            final Bundle bundleTable = context.getBundle();
            final String filterColumnI18n = ((filterColumn == null) ? null : bundleTable.getString(filterColumn));
            actionFilter.update(NameTypeValuesU.create("filter.filterType.column", filterColumnI18n));
            // filter view (form submit buttons)
            final XedPropertyPageView filterView = new XedPropertyPageView(null, cursorFilter);
            final String submitID = context.getSubmitID();
            final String cursorType = context.getViewState().getName();
            final Bundle bundleXed = actionFilter.getXed().getBundle();
            final ActionFactory factory = new ActionFactory(
                    submitID, bundleXed, App.Target.VIEW_STATE, cursorType, null);
            final Collection<String> actions = CollectionU.toCollection(ViewState.Action.FILTER);
            final ActionButtons buttons = factory.create(null, false, actions);
            new PropertyStripHtmlView(filterView, buttons).addContentForm(html);
        }
    }
}
