package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;

public final class LFSSymlinkView extends LFSView {

    public LFSSymlinkView(final LFSRequest request, final AppUserState userState,
                          final File folderBase, final File file) {
        super(request, userState, folderBase, file);
    }

    @Override
    protected HttpResponse addContentTo(final Element html) throws IOException {
        final String basePathContext = getFolderBase().toPath().toRealPath().toString();
        final String requestPathReal = getFile().toPath().toRealPath().toString();
        final String baseURIFolder = getRequest().getBaseURIFolder();

        final HttpResponse httpResponse;
        if (requestPathReal.startsWith(basePathContext)) {
            final String location = baseURIFolder + requestPathReal.substring(basePathContext.length());
            httpResponse = HttpResponseU.to302(location);
        } else {
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }

/*
    private HttpResponse addContentToDeprecate(final Element html) throws IOException {
        final RowSetMetaData metaData = LFSFolder.createMetaData();
        final Locus locus = getUserState().getLocus();
        final ViewStates viewStates = getUserState().getViewStates();
        final ViewState viewState = viewStates.getViewState(metaData, getRequest().getBundle(), locus);
        final LFSRequest request = getRequest();
        final AppUserState userState = getUserState();
        final RowSet rowSetRaw = createRowSetRaw(metaData);
        final RowSet rowSetStyled = new LFSFolderStyled(request, rowSetRaw).getRowSet();
        final Table table = new Table(rowSetStyled, viewState.getSorts(), viewState.getFilters(),
                request.getTitlePath(), request.getTitlePath());
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, request.getBundle(), userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    private RowSet createRowSetRaw(final RowSetMetaData metaData) {
        final boolean viewDot = PropertiesU.isBoolean(getUserState().getProperties(), App.Mode.VIEW_DOT);
        final String path = getRequest().getPath();
        final LFSDataSource source = new LFSDataSource(getRequest(), getFolderBase());
        final File[] files = source.lsSymlink(path);
        final RowSet rowSet = new LFSFolder(getFolderBase(), path, files, metaData, viewDot, false).getRowSet();
        rowSet.getProperties().setProperty(Integer.toString(App.FS.S_IFLNK), Boolean.TRUE.toString());
        return rowSet;
    }
*/
}
