package io.github.greyp9.arwo.app.cron.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cron.service.CronService;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.IOException;

public class CronTabView {
    private final CronService cronService;
    private final AppRequest request;
    private final AppUserState userState;

    public CronTabView(final CronService cronService, final AppRequest request, final AppUserState userState) {
        this.cronService = cronService;
        this.request = request;
        this.userState = userState;
    }

    public final void addContent(final Element html) throws IOException {
        final RowSet rowSet = cronService.createExecutorRowSet(null, request.getHttpRequest().getDate());
        final RowSetMetaData metaData = rowSet.getMetaData();
        final Bundle bundle = request.getBundle();
        final Locus locus = request.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, request.getSubmitID(), App.CSS.TABLE, bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }
}
