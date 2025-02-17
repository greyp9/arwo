package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.cell.TableViewLinks;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.core.KubeU;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Node;
import io.kubernetes.client.openapi.models.V1NodeList;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.time.OffsetDateTime;
import java.util.Arrays;

public class KubeNodesView extends KubeView {

    public KubeNodesView(final ServletHttpRequest httpRequest,
                         final AppUserState userState,
                         final KubeConnectionResource resource) {
        super(httpRequest, userState, resource);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        //final String namespace = resource.getNamespace();
        try {
            final V1NodeList v1NodeList = api.listNode(
                    null, null, null, null, null, null, null, null, null, null, null);
            final RowSet rowSet = loadRowSet(createMetaData(), v1NodeList);
            final UserStateTable table = new UserStateTable(getUserState(), null, getHttpRequest().getDate());
            table.toTableView(rowSet).addContentTo(html);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getResponseBody()));
        }
        return null;
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final V1NodeList v1NodeList) {
        final String baseURI = getHttpRequest().getHttpRequest().getResource();
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final V1Node v1Node : v1NodeList.getItems()) {
            loadRow(rowSet, baseURI, v1Node);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData(FIELD_SELECT, Types.DATALINK),
                new ColumnMetaData(FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(FIELD_CREATED, Types.TIMESTAMP),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final String baseURI, final V1Node v1Node) {
        final V1ObjectMeta metadata = v1Node.getMetadata();
        final String name = (metadata == null) ? null : metadata.getName();
        final OffsetDateTime creationTimestamp = (metadata == null) ? null : metadata.getCreationTimestamp();
        final String hrefPods = PathU.toDir(baseURI, name, CONTEXT_PODS);
        final String hrefDescribe = PathU.toDir(baseURI, name, CONTEXT_DESCRIBE);

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLinks(Arrays.asList(
                new TableViewLink(UTF16.SELECT, CONTEXT_PODS, hrefPods),
                new TableViewLink(UTF16.SELECT, CONTEXT_DESCRIBE, hrefDescribe))));
        insertRow.setNextColumn((metadata == null) ? null : metadata.getName());
        insertRow.setNextColumn((metadata == null) ? null : KubeU.toDate(creationTimestamp));
        rowSet.add(insertRow.getRow());
    }

    private static final String TABLE_ID = "kubePodListType";
}
