package io.github.greyp9.arwo.app.ssh.sftp.view;

import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolder;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolderStyled;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnectionX;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.concurrent.ExecutorService;

public class SFTPFolderView extends SFTPView {

    public SFTPFolderView(
            final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        super(request, userState, resource);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final SFTPRequest request = getRequest();
        final AppUserState userState = getUserState();
        // properties of cursor resource
        addFileProperties(html, null);
        // folder content
        final RowSetMetaData metaData = SFTPFolder.createMetaData();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, request.getBundle(), locus);
        final RowSet rowSetRaw = getRowSetRaw(metaData, viewState);
        final RowSet rowSetStyled = new SFTPFolderStyled(request, rowSetRaw).getRowSet();
        final Table table = new Table(rowSetStyled, viewState.getSorts(), viewState.getFilters(),
                request.getTitlePath(), request.getTitlePath());
        final TableContext tableContext = new TableContext(
                viewState, userState.getSubmitID(), "table", request.getBundle(), userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    private RowSet getRowSetRaw(final RowSetMetaData metaData, final ViewState viewState) throws IOException {
        RowSet rowSet;
        final SFTPRequest request = getRequest();
        final AppUserState userState = getUserState();
        final SSHConnectionResource resource = getResource();
        final SSHConnection sshConnection = resource.getSSHConnection();
        final UserExecutor userExecutor = userState.getUserExecutor();
        final ExecutorService executorStream = userExecutor.getExecutorStream();
        final SSHConnectionX sshConnectionX = new SSHConnectionX(sshConnection, executorStream);
        final ResourceCache cache = userState.getCache();
        final String path = request.getPath();
        // if disconnected, resource will only be fetched if no cached copy is available
        if (viewState.isConnected()) {
            final SFTPDataSource source = new SFTPDataSource(request, sshConnection);
            final Collection<SFTPv3DirectoryEntry> directoryEntries = source.ls(request.getPath());
            rowSet = new SFTPFolder(directoryEntries, metaData, true, sshConnectionX).getRowSet();
        } else if (cache.containsRowSet(path)) {
            rowSet = cache.getRowSet(path);
        } else {
            final SFTPDataSource source = new SFTPDataSource(request, sshConnection);
            final Collection<SFTPv3DirectoryEntry> directoryEntries = source.ls(request.getPath());
            rowSet = new SFTPFolder(directoryEntries, metaData, true, sshConnectionX).getRowSet();
            cache.putRowSet(path, rowSet);
        }
        return rowSet;
    }
}
