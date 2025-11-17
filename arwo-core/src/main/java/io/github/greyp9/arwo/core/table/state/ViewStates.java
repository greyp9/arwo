package io.github.greyp9.arwo.core.table.state;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.filter.Filter;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.w3c.dom.Document;

import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.PatternSyntaxException;

public class ViewStates {
    private final Map<String, ViewState> mapViewState;
    private final XedActionFilter actionFilter;
    private final Xed config;

    public ViewStates(final XedActionFilter actionFilter, final Xed config) {
        this.mapViewState = new TreeMap<String, ViewState>();
        this.actionFilter = actionFilter;
        this.config = config;
    }

    public final ViewState getViewState(final RowSetMetaData metaData, final Bundle bundle, final Locus locus) {
        final ViewState viewState = getViewState(metaData.getID());
        return viewState.normalize(metaData, bundle, locus);
    }

    public final ViewState getViewState(final String id) {
        final boolean exists = mapViewState.containsKey(id);
        return (exists) ? mapViewState.get(id) : addViewState(mapViewState, id);
    }

    private static ViewState addViewState(final Map<String, ViewState> viewStates, final String id) {
        final ViewState viewState = new ViewState(id);
        viewStates.put(id, viewState);
        return viewState;
    }

    @SuppressWarnings({ "PMD.CyclomaticComplexity", "PMD.StdCyclomaticComplexity", "PMD.ModifiedCyclomaticComplexity" })
    public final void apply(final SubmitToken token, final NameTypeValues nameTypeValues,
                            final Bundle bundle, final Alerts alerts) throws IOException {
        final ViewState viewState = getViewState(token.getObject());
        final String action = token.getAction();
        final String message = bundle.getString("alert.action.not.implemented");
        if (action == null) {
            viewState.getClass();

        } else if (ViewState.Action.RESET.equals(action)) {
            viewState.reset();
        } else if (ViewState.Action.SORT.equals(action)) {
            viewState.getSorts().add(token.getObject2());
        } else if (ViewState.Action.FILTER.equals(action)) {
            applyFilter(token, nameTypeValues, bundle, viewState, alerts);
        } else if (ViewState.Action.APPLY_FILTER.equals(action)) {
            applyFilterFavorite(token, bundle, viewState, alerts);
        } else if (ViewState.Action.HIDE.equals(action)) {
            viewState.getHiddenColumns().add(token.getObject2());

        } else if (ViewState.Toggle.CONNECT.equals(action)) {
            viewState.setConnected(!viewState.isConnected());
        } else if (ViewState.Toggle.RIBBON.equals(action)) {
            final boolean openTH = viewState.isOpenTH();
            final boolean openTB = viewState.isOpenTB();
            viewState.setOpenTH(!openTH && openTB);
            viewState.setOpenTB(!openTH || !openTB);
        } else if (ViewState.Toggle.BASELINE.equals(action)) {
            viewState.setActiveBaseline(!viewState.isActiveBaseline());
        } else if (ViewState.Toggle.FILTERS.equals(action)) {
            viewState.setFilterColumn(Value.join(Http.Token.DOT, token.getObject(), token.getObject2()));
        } else if (ViewState.Toggle.PAGE.equals(action)) {
            final Preferences preferences = new Preferences(config);
            viewState.setPage(Page.Factory.togglePage(viewState.getPage(), preferences.getTablePageSize()));
        } else if (ViewState.Nav.FIRST.equals(action)) {
            viewState.setPage(Page.Factory.firstPage(viewState.getPage()));
        } else if (ViewState.Nav.PREVIOUS.equals(action)) {
            viewState.setPage(Page.Factory.prevPage(viewState.getPage()));
        } else if (ViewState.Nav.NEXT.equals(action)) {
            viewState.setPage(Page.Factory.nextPage(viewState.getPage()));
        } else if (ViewState.Nav.LAST.equals(action)) {
            viewState.setPage(Page.Factory.lastPage(viewState.getPage()));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
    }

    private void applyFilter(final SubmitToken token, final NameTypeValues nameTypeValues, final Bundle bundle,
                             final ViewState viewState, final Alerts alerts) throws IOException {
        try {
            Filter filter = actionFilter.getFilter(nameTypeValues);
            final String baseline = ViewState.Toggle.BASELINE;
            final String i18nKey = Value.join(Http.Token.DOT, token.getObject(), baseline);
            final String i18nValue = bundle.getString(i18nKey);
            if (filter.getName().equals(i18nValue)) {
                filter = new Filter(filter.getIndex(), baseline, filter.getOperator(), filter.getValue());
            }
            viewState.getFilters().add(filter);
        } catch (PatternSyntaxException e) {
            alerts.add(new Alert(Alert.Severity.WARN, e.getMessage(), token.toString()));
        }
    }

    private void applyFilterFavorite(final SubmitToken token,
                                     final Bundle bundle,
                                     final ViewState viewState,
                                     final Alerts alerts) throws IOException {
        final SubmitToken tokenTranslate = new SubmitToken(
                token.getSubject(), ViewState.Action.FILTER, token.getObject());
        final NameTypeValues ntvTranslate = new NameTypeValues();
        final String filterXml = token.getObject2();
        final URL urlInitial = ResourceU.resolve(App.Actions.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final Document document = DocumentU.toDocument(filterXml);
        final Xed xed = new Xed(document, xsdTypes);
        final XedCursor cursorFilter = new XedNav(xed).find(document.getDocumentElement());
        final TypeInstance typeInstanceFilter = cursorFilter.getTypeInstance();
        final Collection<TypeInstance> typeInstances = typeInstanceFilter.getDataType().getInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            String key = typeInstanceIt.getID(typeInstanceFilter);
            String value = cursorFilter.getValue(typeInstanceIt);
            ntvTranslate.add(key, value);
        }
        applyFilter(tokenTranslate, ntvTranslate, bundle, viewState, alerts);
    }
}
