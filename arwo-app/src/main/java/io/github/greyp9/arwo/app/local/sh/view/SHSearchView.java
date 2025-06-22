package io.github.greyp9.arwo.app.local.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.app.local.sh.data.LSHHistoryRowSetSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.cache.CacheRowSetSource;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@SuppressWarnings("PMD.ExcessiveImports")
public class SHSearchView extends SHView {

    public SHSearchView(final SHRequest request, final AppUserState userState) {
        super(request, userState);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final AppUserState userState = getUserState();
        final Alerts alerts = userState.getAlerts();
        final File folderHistory = new File(userState.getUserRoot(), "result/lsh");
        final FindInFolderQuery findInFolderQuery = new FindInFolderQuery(folderHistory, "*.results", true);
        final List<File> files = new ArrayList<>(findInFolderQuery.getFound());
        // sort by date DESC, so we get most recent usage of each command
        files.sort(Comparator.comparing(File::lastModified).reversed());
        alerts.add(new Alert(Alert.Severity.INFO, String.format("Query LSH History; %d entries", files.size())));
        // render history table
        try {
            final RowSetSource rowSetSource = new CacheRowSetSource(getUserState().getCache(),
                    new LSHHistoryRowSetSource(files), httpRequest.getURI());
            final RowSet rowSet = rowSetSource.getRowSet();
            final UserStateTable table = new UserStateTable(getUserState(), null, httpRequest.getDate(), true);
            table.toTableView(rowSet).addContentTo(html);
        } catch (Exception e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }
}
