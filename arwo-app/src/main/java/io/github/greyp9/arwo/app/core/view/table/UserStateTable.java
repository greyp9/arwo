package io.github.greyp9.arwo.app.core.view.table;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;

import java.io.IOException;
import java.util.Date;

public final class UserStateTable {
    private final AppUserState userState;
    private final String title;
    private final Date date;
    private final boolean refreshControl;

    public UserStateTable(final AppUserState userState, final String title, final Date date) {
        this(userState, title, date, false);
    }

    public UserStateTable(final AppUserState userState, final String title, final Date date,
                          final boolean refreshControl) {
        this.userState = userState;
        this.title = title;
        this.date = date;
        this.refreshControl = refreshControl;
    }

    public TableView toTableView(final RowSet rowSet) throws IOException {
        final ViewState viewState = userState.getViewStates().getViewState(
                rowSet.getMetaData(), userState.getBundle(), userState.getLocus());
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), title, title);
        TableU.addFooterStandard(table, userState.getBundle());
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(viewState, filter, userState.getSubmitID(),
                App.CSS.TABLE, refreshControl, userState.getBundle(), userState.getLocus(), date);
        return new TableView(table, tableContext);
    }
}
