package io.github.greyp9.arwo.app.ssh.sftp.view;

import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolder;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolderStyled;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;

public class SFTPFolderView extends SFTPView {

    public SFTPFolderView(
            final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        super(request, userState, resource);
    }

    public final HttpResponse addContentTo(final Element html) throws IOException {
        final SFTPRequest request = getRequest();
        final AppUserState userState = getUserState();
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
        final SFTPRequest request = getRequest();
        final SSHConnectionResource resource = getResource();
        RowSet rowSet;
        //ResourceCache cache = userState.getCache();
        //String uri = request.getHttpRequest().getURI();
        // if disconnected, resource will only be fetched if no cached copy is available
        if (viewState.isConnected()) {
            final SFTPDataSource source = new SFTPDataSource(request, resource.getSSHConnection());
            final Collection<SFTPv3DirectoryEntry> directoryEntries = source.ls(request.getPath());
            rowSet = new SFTPFolder(directoryEntries, metaData, true).getRowSet();
            //} else if (cache.containsRowSet(uri)) {
            //    rowSet = cache.getRowSet(uri);
        } else {
            final SFTPDataSource source = new SFTPDataSource(request, resource.getSSHConnection());
            final Collection<SFTPv3DirectoryEntry> directoryEntries = source.ls(request.getPath());
            rowSet = new SFTPFolder(directoryEntries, metaData, true).getRowSet();
            //cache.putRowSet(uri, rowSet);
        }
        return rowSet;
    }
}
