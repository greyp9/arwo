package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collection;

public class KubeEndpointView extends KubeView {

    public KubeEndpointView(final ServletHttpRequest httpRequest,
                            final AppUserState userState,
                            final KubeConnectionResource resource) {
        super(httpRequest, userState, resource);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        // get reference to configured kube endpoints
        final Xed xed = getUserState().getDocumentState().getSession(App.Servlet.SETTINGS).getXed();
        final Element element = xed.getXPather().getElement("/app:app/app:kubes");
        final XedCursor cursor = (element == null) ? null : new XedNav(xed).find(element);
        if (cursor == null) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.INFO, "no.kubernetes.configuration"));
        } else {
            final RowSet rowSet = loadRowSet(createMetaData(), cursor);
            final UserStateTable table = new UserStateTable(getUserState(), null, getHttpRequest().getDate());
            table.toTableView(rowSet).addContentTo(html);
        }
        return null;
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final XedCursor cursorKubes) {
        final String baseURI = PathU.toPath(getHttpRequest().getBaseURI());
        final RowSet rowSet = new RowSet(metaData, null, null);
        final Collection<Element> children = cursorKubes.getChildren(cursorKubes.getChildInstance("kube"));
        for (Element child : children) {
            final XedCursor cursorKube = new XedNav(cursorKubes.getXed()).find(child);
            loadRow(rowSet, baseURI, cursorKube);
        }
        return rowSet;
    }


    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(FIELD_SELECT, Types.DATALINK),
                new ColumnMetaData(FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(FIELD_COMMENT, Types.VARCHAR),
                new ColumnMetaData(FIELD_CONTEXT, Types.VARCHAR),
                new ColumnMetaData(FIELD_NAMESPACE, Types.VARCHAR),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final String baseURI, final XedCursor cursor) {
        final boolean isEnabled = TypeU.toBooleanP(cursor.getValue(cursor.getChildInstance(FIELD_ENABLED)));
        if (isEnabled) {
            final String name = cursor.getValue(cursor.getChildInstance(FIELD_NAME));
            final String comment = cursor.getValue(cursor.getChildInstance(FIELD_COMMENT));
            final String context = cursor.getValue(cursor.getChildInstance(FIELD_CONTEXT));
            final String namespace = cursor.getValue(cursor.getChildInstance(FIELD_NAMESPACE));
            final String href = PathU.toDir(baseURI, name, CONTEXT_PODS);

            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, name, href));
            insertRow.setNextColumn(name);
            insertRow.setNextColumn(comment);
            insertRow.setNextColumn(context);
            insertRow.setNextColumn(namespace);
            rowSet.add(insertRow.getRow());
        }
    }

    private static final String FIELD_SELECT = "select";
    private static final String FIELD_NAME = "name";
    private static final String FIELD_COMMENT = "comment";
    private static final String FIELD_ENABLED = "enabled";
    private static final String FIELD_CONTEXT = "context";
    private static final String FIELD_NAMESPACE = "namespace";
    private static final String CONTEXT_PODS = "pods";
    private static final String TABLE_ID = "kubeEndpointType";
}
