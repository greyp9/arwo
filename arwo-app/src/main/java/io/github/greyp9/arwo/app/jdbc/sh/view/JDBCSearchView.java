package io.github.greyp9.arwo.app.jdbc.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.app.jdbc.sh.core.JDBCRequest;
import io.github.greyp9.arwo.app.jdbc.sh.data.JDBCHistoryRowSetSource;
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

public class JDBCSearchView extends JDBCView {

    public JDBCSearchView(final JDBCRequest request, final AppUserState userState) {
        super(request, userState);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        // request context
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final AppUserState userState = getUserState();
        final Alerts alerts = userState.getAlerts();
        // query data store
        final File folderHistory = new File(userState.getUserRoot(), "result/jdbc");
        final FindInFolderQuery findInFolderQuery = new FindInFolderQuery(folderHistory, "*.results", true);
        final List<File> files = new ArrayList<>(findInFolderQuery.getFound());
        // sort by date DESC, so we get most recent usage of each command
        files.sort(Comparator.comparing(File::lastModified).reversed());
        alerts.add(new Alert(Alert.Severity.INFO, String.format("Query JDBC History; %d entries", files.size())));
        // render history table
        try {
            final RowSetSource rowSetSource = new CacheRowSetSource(getUserState().getCache(),
                    new JDBCHistoryRowSetSource(files), JDBCHistoryRowSetSource.ROWSET_ID);
            final RowSet rowSet = rowSetSource.getRowSet();
            final UserStateTable table = new UserStateTable(getUserState(), null, httpRequest.getDate(), true);
            table.toTableView(rowSet).addContentTo(html);
        } catch (Exception e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }
}
