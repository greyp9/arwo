package io.github.greyp9.arwo.core.table.state;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

public class ViewStates {

    private final Map<String, ViewState> mapViewState;

    public ViewStates() {
        this.mapViewState = new TreeMap<String, ViewState>();
    }

    public final ViewState getViewState(final RowSetMetaData metaData, final Bundle bundle) {
        final ViewState viewState = getViewState(metaData.getID());
        return viewState.normalize(metaData, bundle);
    }

    private ViewState getViewState(final String id) {
        final boolean exists = mapViewState.containsKey(id);
        return (exists) ? mapViewState.get(id) : addViewState(mapViewState, id);
    }

    private static ViewState addViewState(final Map<String, ViewState> viewStates, final String id) {
        final ViewState viewState = new ViewState(id);
        viewStates.put(id, viewState);
        return viewState;
    }

    @SuppressWarnings({ "PMD.CyclomaticComplexity", "PMD.StdCyclomaticComplexity", "PMD.ModifiedCyclomaticComplexity" })
    public final void apply(final SubmitToken token, final NameTypeValues nameTypeValues) throws IOException {
        final ViewState viewState = getViewState(token.getObject());
        final String action = token.getAction();
        if (action == null) {
            viewState.getClass();

        } else if (ViewState.Action.RESET.equals(action)) {
            viewState.reset();
        } else if (ViewState.Action.SORT.equals(action)) {
            viewState.getSorts().add(token.getObject2());
        } else if (ViewState.Action.FILTER.equals(action)) {
            viewState.getFilters().add(new XedActionFilter(null).getFilter(nameTypeValues));
        } else if (ViewState.Action.HIDE.equals(action)) {
            viewState.getHiddenColumns().add(token.getObject2());

        } else if (ViewState.Toggle.CONNECT.equals(action)) {
            viewState.setIsConnected(!viewState.isConnected());
        } else if (ViewState.Toggle.RIBBON.equals(action)) {
            viewState.setIsOpenTH(!viewState.isOpenTH());
        } else if (ViewState.Toggle.BASELINE.equals(action)) {
            viewState.setIsActiveBaseline(!viewState.isActiveBaseline());
        } else if (ViewState.Toggle.FILTERS.equals(action)) {
            viewState.setFilterColumn(Value.join(Http.Token.DOT, token.getObject(), token.getObject2()));
        } else if (ViewState.Toggle.PAGE.equals(action)) {
            viewState.setPage(Page.Factory.togglePage(viewState.getPage(), Const.PAGE_SIZE_TABLE));
        } else if (ViewState.Nav.FIRST.equals(action)) {
            viewState.setPage(Page.Factory.firstPage(viewState.getPage()));
        } else if (ViewState.Nav.PREVIOUS.equals(action)) {
            viewState.setPage(Page.Factory.prevPage(viewState.getPage()));
        } else if (ViewState.Nav.NEXT.equals(action)) {
            viewState.setPage(Page.Factory.nextPage(viewState.getPage()));
        } else if (ViewState.Nav.LAST.equals(action)) {
            viewState.setPage(Page.Factory.lastPage(viewState.getPage()));
        }
    }

    private static class Const {
        private static final int PAGE_SIZE_TABLE = 30;
    }
}
