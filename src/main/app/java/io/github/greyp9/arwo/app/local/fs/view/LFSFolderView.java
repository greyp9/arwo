package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.create.AppFileCreateView;
import io.github.greyp9.arwo.app.core.view.create.AppFolderCreateView;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.app.local.fs.data.LFSFolder;
import io.github.greyp9.arwo.app.local.fs.data.LFSFolderStyled;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.result.io.ResultsPersister;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Date;

public class LFSFolderView extends LFSView {

    public LFSFolderView(
            final LFSRequest request, final AppUserState userState, final File folderBase, final File file) {
        super(request, userState, folderBase, file);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final LFSRequest request = getRequest();
        final String mode = request.getMode();
        // resource access (read versus write)
        if (App.Mode.CREATE_F.equals(mode)) {
            doGetFileCreate(html);
        } else if (App.Mode.CREATE_D.equals(mode)) {
            doGetFolderCreate(html);
        }
        return doGetFolder(html);
    }

    private HttpResponse doGetFileCreate(final Element html) throws IOException {
        final LFSRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        return new AppFileCreateView(httpRequest, userState).addContentTo(html);
    }

    private HttpResponse doGetFolderCreate(final Element html) throws IOException {
        final LFSRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        return new AppFolderCreateView(httpRequest, userState).addContentTo(html);
    }

    private HttpResponse doGetFolder(final Element html) throws IOException {
        final LFSRequest request = getRequest();
        final AppUserState userState = getUserState();
        // properties of cursor resource
        addFileProperties(html, null);
        // folder content
        final RowSetMetaData metaData = LFSFolder.createMetaData();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, request.getBundle(), locus);
        final boolean findMode = App.Action.FIND.equals(request.getMode());
        if (findMode) {
            viewState.getHiddenColumns().remove("folder");  // i18n metadata
        } else {
            viewState.getHiddenColumns().add("folder");  // i18n metadata
        }
        final RowSet rowSetRaw = getRowSetRaw(metaData, viewState, findMode);
        // protect against unexpected large result set
        final int pageSize = new Preferences(getUserState().getConfig()).getTablePageSize();
        boolean isLargeRowSet = (rowSetRaw.getRows() > pageSize);
        if ((isLargeRowSet) && (viewState.getPage() == null) && (viewState.isAutoPage())) {
            viewState.setPage(Page.Factory.togglePage(viewState.getPage(), pageSize));
            userState.getAlerts().add(new Alert(Alert.Severity.INFO, getBundle().getString("table.autopage")));
            viewState.setAutoPage(false);
        }
        final RowSet rowSetStyled = new LFSFolderStyled(request, rowSetRaw).getRowSet();
        // optionally persist fetched results
        final Results results = new Results(request.getContext(), request.getHttpRequest().getURI(),
                new Interval(request.getHttpRequest().getDate(), new Date()));
        results.add(rowSetStyled.getID(), null, rowSetStyled);
        // optionally persist fetched results
        new ResultsPersister(request.getUserState().getResultsContext(request.getHttpRequest())).write(results);
        // render for response
        final Table table = new Table(rowSetStyled, viewState.getSorts(), viewState.getFilters(),
                request.getTitlePath(), request.getTitlePath());
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(viewState, filter, userState.getSubmitID(),
                App.CSS.TABLE, request.getBundle(), userState.getLocus(), request.getHttpRequest().getDate());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    private RowSet getRowSetRaw(
            final RowSetMetaData metaData, final ViewState viewState, final boolean recurse) throws IOException {
        RowSet rowSet;
        final LFSRequest request = getRequest();
        final AppUserState userState = getUserState();
        final File folderBase = getFolderBase();
        final LFSDataSource source = new LFSDataSource(request, folderBase);
        final ResourceCache cache = userState.getCache();
        final String path = request.getPath();
        final boolean viewDot = PropertiesU.isBoolean(userState.getProperties(), App.Mode.VIEW_DOT);
        // on view state change, userState at next HTTP/GET will contain USE_CACHE directive
        // if disconnected, resource will only be fetched if no cached copy is available
        final boolean cached = cache.containsRowSet(path);
        if (cached && PropertiesU.isBoolean(userState.getProperties(), App.Action.USE_CACHE)) {
            rowSet = cache.getRowSet(path);
        } else if (viewState.isConnected()) {
            final File[] files = source.listFiles(path, recurse);
            rowSet = new LFSFolder(folderBase, path, files, metaData, viewDot, true).getRowSet();
            cache.putRowSet(path, rowSet);
        } else if (cached) {
            rowSet = cache.getRowSet(path);
        } else {
            final File[] files = source.listFiles(path, recurse);
            rowSet = new LFSFolder(folderBase, path, files, metaData, viewDot, true).getRowSet();
            cache.putRowSet(path, rowSet);
        }
        return rowSet;
    }
}
