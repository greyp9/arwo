package io.github.greyp9.arwo.app.ssh.sftp.view;

import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.create.AppFileCreateView;
import io.github.greyp9.arwo.app.core.view.create.AppFolderCreateView;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolder;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolderStyled;
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
import io.github.greyp9.arwo.core.util.PropertiesX;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnectionX;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.ExecutorService;

@SuppressWarnings("PMD.ExcessiveImports")
public class SFTPFolderView extends SFTPView {

    public SFTPFolderView(
            final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        super(request, userState, resource);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final SFTPRequest request = getRequest();
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
        final SFTPRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        return new AppFileCreateView(httpRequest, userState).addContentTo(html);
    }

    private HttpResponse doGetFolderCreate(final Element html) throws IOException {
        final SFTPRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        return new AppFolderCreateView(httpRequest, userState).addContentTo(html);
    }

    private HttpResponse doGetFolder(final Element html) throws IOException {
        final SFTPRequest request = getRequest();
        final AppUserState userState = getUserState();
        // properties of cursor resource
        addFileProperties(html, null);
        // folder content
        final RowSetMetaData metaData = SFTPFolder.createMetaData();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, request.getBundle(), locus);
        final boolean findMode = PropertiesU.isBoolean(userState.getProperties(), App.Action.FIND);
        if (findMode) {
            viewState.getHiddenColumns().remove("folder");  // i18n metadata
        } else {
            viewState.getHiddenColumns().add("folder");  // i18n metadata
        }
        final RowSet rowSetRaw = (findMode ?
                getRowSetRawFind(metaData, viewState) :
                getRowSetRaw(metaData, viewState));
        final RowSet rowSetStyled = new SFTPFolderStyled(request, rowSetRaw).getRowSet();
        // optionally persist fetched results
        final Results results = new Results(request.getHttpRequest().getURI(),
                new Interval(request.getHttpRequest().getDate(), new Date()));
        results.add(rowSetStyled.getID(), null, rowSetStyled);
        // optionally persist fetched results
        new ResultsPersister(getUserState().getResultsContext(getRequest().getHttpRequest())).write(results);
        // render for response
        final Table table = new Table(rowSetStyled, viewState.getSorts(), viewState.getFilters(),
                request.getTitlePath(), request.getTitlePath());
        final TableContext tableContext = new TableContext(
                viewState, userState.getSubmitID(), App.CSS.TABLE, request.getBundle(), userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    private RowSet getRowSetRaw(final RowSetMetaData metaData, final ViewState viewState) throws IOException {
        RowSet rowSet;
        final SFTPRequest request = getRequest();
        final AppUserState userState = getUserState();
        final SSHConnectionResource resource = getResource();
        final SSHConnection connection = resource.getConnection();
        final UserExecutor userExecutor = userState.getUserExecutor();
        final ExecutorService executorStream = userExecutor.getExecutorStream();
        final SSHConnectionX sshConnectionX = new SSHConnectionX(connection, executorStream);
        final ResourceCache cache = userState.getCache();
        final String path = request.getPath();
        // if disconnected, resource will only be fetched if no cached copy is available
        if (viewState.isConnected()) {
            final SFTPDataSource source = new SFTPDataSource(request, connection);
            final Collection<SFTPv3DirectoryEntry> directoryEntries = source.ls(path);
            rowSet = new SFTPFolder(null, directoryEntries, metaData, true, sshConnectionX).getRowSet();
            cache.putRowSet(path, rowSet);
        } else if (cache.containsRowSet(path)) {
            rowSet = cache.getRowSet(path);
        } else {
            final SFTPDataSource source = new SFTPDataSource(request, connection);
            final Collection<SFTPv3DirectoryEntry> directoryEntries = source.ls(path);
            rowSet = new SFTPFolder(null, directoryEntries, metaData, true, sshConnectionX).getRowSet();
            cache.putRowSet(path, rowSet);
        }
        return rowSet;
    }

    private RowSet getRowSetRawFind(final RowSetMetaData metaData, final ViewState viewState) throws IOException {
        RowSet rowSet;
        final SFTPRequest request = getRequest();
        final AppUserState userState = getUserState();
        final ResourceCache cache = userState.getCache();
        final String path = request.getPath();
        // if disconnected, resource will only be fetched if no cached copy is available
        if (viewState.isConnected()) {
            rowSet = getRowSetRawFindRemote(metaData);
            cache.putRowSet(path, rowSet);
        } else if (cache.containsRowSet(path)) {
            rowSet = cache.getRowSet(path);
        } else {
            rowSet = getRowSetRawFindRemote(metaData);
            cache.putRowSet(path, rowSet);
        }
        return rowSet;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private RowSet getRowSetRawFindRemote(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSetComposite = new RowSet(metaData, null, null);
        final PropertiesX propertiesC = new PropertiesX(rowSetComposite.getProperties());
        final SFTPRequest request = getRequest();
        final AppUserState userState = getUserState();
        final SSHConnectionResource resource = getResource();
        final SSHConnection connection = resource.getConnection();
        final SFTPDataSource source = new SFTPDataSource(request, connection);
        final String path = request.getPath();
        final UserExecutor userExecutor = userState.getUserExecutor();
        final ExecutorService executorStream = userExecutor.getExecutorStream();
        final SSHConnectionX sshConnectionX = new SSHConnectionX(connection, executorStream);
        final Map<String, Collection<SFTPv3DirectoryEntry>> map = source.find(path);
        for (final Map.Entry<String, Collection<SFTPv3DirectoryEntry>> entry : map.entrySet()) {
            final String folder = entry.getKey();
            final String subfolder = folder.substring(path.length());
            final Collection<SFTPv3DirectoryEntry> directoryEntries = entry.getValue();
            final RowSet rowSet = new SFTPFolder(
                    subfolder, directoryEntries, metaData, true, sshConnectionX).getRowSet();
            rowSet.updateOrdinals(rowSetComposite.getRows());
            rowSetComposite.addAll(rowSet);
            propertiesC.addAll(rowSet.getProperties());
        }
        return rowSetComposite;
    }
}
