package io.github.greyp9.arwo.app.cron.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.meter.Meter;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import org.w3c.dom.Element;

import java.io.IOException;

public class CronInvocationView {
    private final AppRequest request;
    private final AppUserState userState;

    public CronInvocationView(final AppRequest request, final AppUserState userState) {
        this.request = request;
        this.userState = userState;
    }

    public final void addContent(final Element html) throws IOException {
        final Meter meterCron = userState.getCron().getMeterCron();
        final Bundle bundle = meterCron.getXed().getBundle();
        final RowSet rowSet = userState.getCron().getRowSetCron();
        final RowSetMetaData metaData = rowSet.getMetaData();
        final Locus locus = request.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(
                viewState, userState.getSubmitID(), "table", bundle, request.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }
}
