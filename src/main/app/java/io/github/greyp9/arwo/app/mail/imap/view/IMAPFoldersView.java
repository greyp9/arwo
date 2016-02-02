package io.github.greyp9.arwo.app.mail.imap.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.mail.imap.core.IMAPRequest;
import io.github.greyp9.arwo.app.mail.imap.data.IMAPDataSource;
import io.github.greyp9.arwo.app.mail.imap.data.IMAPFoldersStyled;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.result.io.ResultsPersister;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Date;

public class IMAPFoldersView extends IMAPView {

    public IMAPFoldersView(final IMAPRequest request, final AppUserState userState) {
        super(request, userState);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final IMAPRequest request = getRequest();
        final AppUserState userState = request.getUserState();
        final RowSetMetaData metaData = IMAPDataSource.getFoldersMetaData();
        final ViewState viewState = userState.getViewStates().getViewState(
                metaData, request.getBundle(), request.getLocus());

        final RowSet rowSet = getRowSet(metaData, viewState.isConnected());
        final RowSet rowSetStyled = new IMAPFoldersStyled(request, rowSet).getRowSet();
        // optionally persist fetched results
        final Results results = new Results(request.getHttpRequest().getURI(),
                new Interval(request.getHttpRequest().getDate(), new Date()));
        results.add(rowSetStyled.getID(), null, rowSetStyled);
        new ResultsPersister(request.getUserState().getResultsContext(request.getHttpRequest())).write(results);
        // render for response
        final Table table = new Table(rowSetStyled, viewState.getSorts(), viewState.getFilters(),
                request.getTitlePath(), request.getTitlePath());
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, request.getBundle(), userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    private RowSet getRowSet(final RowSetMetaData metaData, final boolean isConnected) throws IOException {
        RowSet rowSet;
        final IMAPRequest request = getRequest();
        final AppUserState userState = request.getUserState();
        final ResourceCache cache = userState.getCache();
        final String path = request.getHttpRequest().getURI();
        if (isConnected) {
            final IMAPDataSource source = getDataSource();
            rowSet = source.getFolders(metaData);
            cache.putRowSet(path, rowSet);
        } else if (cache.containsRowSet(path)) {
            rowSet = cache.getRowSet(path);
        } else {
            final IMAPDataSource source = getDataSource();
            rowSet = source.getFolders(metaData);
            cache.putRowSet(path, rowSet);
        }
        return rowSet;
    }
}
