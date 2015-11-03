package io.github.greyp9.arwo.core.table.state;

import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

@SuppressWarnings({ "PMD.CyclomaticComplexity", "PMD.StdCyclomaticComplexity", "PMD.ModifiedCyclomaticComplexity" })
public class ViewStates {

    private final Map<String, ViewState> mapViewState;

    public ViewStates() {
        this.mapViewState = new TreeMap<String, ViewState>();
    }

    public final ViewState getViewState(final String id) {
        final boolean exists = mapViewState.containsKey(id);
        return (exists) ? mapViewState.get(id) : addViewState(mapViewState, id);
    }

/*
    public ViewState getViewState(RowSetMetaData metaData, Locus locus) {
        ViewState viewState = getViewState(metaData.getID());
        viewState.localize(metaData, locus);
        return viewState;
    }
*/

    private static ViewState addViewState(final Map<String, ViewState> viewStates, final String id) {
        final ViewState viewState = new ViewState(id);
        viewStates.put(id, viewState);
        return viewState;
    }

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
            nameTypeValues.getClass();
            //viewState.getFilters().add(new XedActionFilter(null).getFilter(nameValues));
        } else if (ViewState.Action.HIDE.equals(action)) {
            viewState.getHiddenColumns().add(token.getObject2());

        } else if (ViewState.Toggle.CONNECT.equals(action)) {
            viewState.setIsConnected(!viewState.isConnected());
        } else if (ViewState.Toggle.RIBBON.equals(action)) {
            viewState.setIsOpenTH(!viewState.isOpenTH());
        } else if (ViewState.Toggle.BASELINE.equals(action)) {
            viewState.setIsActiveBaseline(!viewState.isActiveBaseline());
        } else if (ViewState.Toggle.FILTERS.equals(action)) {
            viewState.setFilterColumn(token.getObject2());
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
