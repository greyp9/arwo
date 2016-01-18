package io.github.greyp9.arwo.app.mail.pop3.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.mail.pop3.core.POP3Request;
import io.github.greyp9.arwo.app.mail.pop3.data.POP3DataSource;
import io.github.greyp9.arwo.app.mail.pop3.data.POP3MessagesStyled;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import org.w3c.dom.Element;

import java.io.IOException;

public class POP3MessagesView extends POP3View {

    public POP3MessagesView(final POP3Request request, final AppUserState userState) {
        super(request, userState);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final POP3Request request = getRequest();
        final AppUserState userState = request.getUserState();
        final RowSetMetaData metaData = POP3DataSource.getMessagesMetaData();
        final ViewState viewState = userState.getViewStates().getViewState(
                metaData, request.getBundle(), request.getLocus());
        viewState.getHiddenColumns().add("message");
        final RowSet rowSet = getRowSet(metaData, viewState.isConnected());
        final RowSet rowSetStyled = new POP3MessagesStyled(request, rowSet).getRowSet();
        final Table table = new Table(rowSetStyled, viewState.getSorts(), viewState.getFilters(),
                request.getTitlePath(), request.getTitlePath());
        final TableContext tableContext = new TableContext(
                viewState, userState.getSubmitID(), App.CSS.TABLE, request.getBundle(), userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    private RowSet getRowSet(final RowSetMetaData metaData, final boolean isConnected) throws IOException {
        RowSet rowSet;
        final POP3Request request = getRequest();
        final AppUserState userState = request.getUserState();
        final ResourceCache cache = userState.getCache();
        final String path = request.getHttpRequest().getURI();
        if (isConnected) {
            final POP3DataSource source = getDataSource();
            rowSet = source.getMessages(metaData, request.getFolder());
            cache.putRowSet(path, rowSet);
        } else if (cache.containsRowSet(path)) {
            rowSet = cache.getRowSet(path);
        } else {
            final POP3DataSource source = getDataSource();
            rowSet = source.getMessages(metaData, request.getFolder());
            cache.putRowSet(path, rowSet);
        }
        return rowSet;
    }
}
