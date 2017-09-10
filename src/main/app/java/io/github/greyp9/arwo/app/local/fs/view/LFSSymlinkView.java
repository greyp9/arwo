package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.app.local.fs.data.LFSFolder;
import io.github.greyp9.arwo.app.local.fs.data.LFSFolderStyled;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;

public class LFSSymlinkView extends LFSView {

    public LFSSymlinkView(LFSRequest request, AppUserState userState, File folderBase, File file) {
        super(request, userState, folderBase, file);
    }

    @Override
    protected HttpResponse addContentTo(Element html) throws IOException {
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

    private RowSet createRowSetRaw(final RowSetMetaData metaData) throws IOException {
        final String path = getRequest().getPath();
        final LFSDataSource source = new LFSDataSource(getRequest(), getFolderBase());
        final File[] files = source.lsSymlink(path);
        final RowSet rowSet = new LFSFolder(getFolderBase(), path, files, metaData, false).getRowSet();
        rowSet.getProperties().setProperty(Integer.toString(App.FS.S_IFLNK), Boolean.TRUE.toString());
        return rowSet;
    }
}
