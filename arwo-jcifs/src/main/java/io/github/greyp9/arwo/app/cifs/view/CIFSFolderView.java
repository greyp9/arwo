package io.github.greyp9.arwo.app.cifs.view;

import io.github.greyp9.arwo.app.cifs.connection.CIFSConnectionResource;
import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.cifs.data.CIFSDataSource;
import io.github.greyp9.arwo.app.cifs.data.CIFSFolder;
import io.github.greyp9.arwo.app.cifs.data.CIFSFolderStyled;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.create.AppFileCreateView;
import io.github.greyp9.arwo.app.core.view.create.AppFolderCreateView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
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
import io.github.greyp9.arwo.lib.jcifs.fs.connection.CIFSConnection;
import jcifs.smb.SmbFile;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Date;

@SuppressWarnings("PMD.ExcessiveImports")
public class CIFSFolderView extends CIFSView {

    public CIFSFolderView(
            final CIFSRequest request, final AppUserState userState, final CIFSConnectionResource resource) {
        super(request, userState, resource);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final CIFSRequest request = getRequest();
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
        final CIFSRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        return new AppFileCreateView(httpRequest, userState).addContentTo(html);
    }

    private HttpResponse doGetFolderCreate(final Element html) throws IOException {
        final CIFSRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        return new AppFolderCreateView(httpRequest, userState).addContentTo(html);
    }

    private HttpResponse doGetFolder(final Element html) throws IOException {
        final CIFSRequest request = getRequest();
        final AppUserState userState = getUserState();
        // properties of cursor resource
        addFileProperties(html, null);
        // folder content
        final RowSetMetaData metaData = CIFSFolder.createMetaData();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, request.getBundle(), locus);
        final boolean findMode = PropertiesU.isBoolean(userState.getProperties(), App.Action.FIND);
        if (findMode) {
            viewState.getHiddenColumns().remove("folder");  // i18n metadata
        } else {
            viewState.getHiddenColumns().add("folder");  // i18n metadata
        }
        final RowSet rowSetRaw = (findMode
                ? getRowSetRawFind(metaData, viewState)
                : getRowSetRaw(metaData, viewState));
        final RowSet rowSetStyled = new CIFSFolderStyled(request, rowSetRaw).getRowSet();
        // optionally persist fetched results
        final Results results = new Results(request.getServer(), request.getHttpRequest().getURI(),
                new Interval(request.getHttpRequest().getDate(), new Date()));
        results.add(rowSetStyled.getID(), null, rowSetStyled);
        // optionally persist fetched results
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

    private RowSet getRowSetRaw(final RowSetMetaData metaData, final ViewState viewState) throws IOException {
        RowSet rowSet;
        final CIFSRequest request = getRequest();
        final AppUserState userState = getUserState();
        final CIFSConnectionResource resource = getResource();
        final CIFSConnection connection = resource.getConnection();
        final ResourceCache cache = userState.getCacheBlob();
        final String path = request.getPath();

        // if disconnected, resource will only be fetched if no cached copy is available
        if (viewState.isConnected()) {
            final CIFSDataSource source = new CIFSDataSource(request, connection);
            final Collection<SmbFile> smbFiles = source.listFiles(path);
            rowSet = new CIFSFolder(null, smbFiles, metaData, true).getRowSet();
            cache.putRowSet(path, rowSet);
        } else if (cache.containsRowSet(path)) {
            rowSet = cache.getRowSet(path);
        } else {
            final CIFSDataSource source = new CIFSDataSource(request, connection);
            final Collection<SmbFile> smbFiles = source.listFiles(path);
            rowSet = new CIFSFolder(null, smbFiles, metaData, true).getRowSet();
            cache.putRowSet(path, rowSet);
        }
        return rowSet;
    }

    private RowSet getRowSetRawFind(final RowSetMetaData metaData, final ViewState viewState) throws IOException {
        return getRowSetRaw(metaData, viewState);
    }
}
