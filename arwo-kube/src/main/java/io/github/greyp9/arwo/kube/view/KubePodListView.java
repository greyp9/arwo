package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.openapi.models.V1Pod;
import io.kubernetes.client.openapi.models.V1PodList;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;

@SuppressWarnings("PMD.ExcessiveImports")
public class KubePodListView extends KubeView {

    @SuppressWarnings("WeakerAccess")
    public KubePodListView(final ServletHttpRequest httpRequest,
                           final AppUserState userState,
                           final KubeConnectionResource resource) {
        super(httpRequest, userState, resource);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        final String namespace = resource.getNamespace();
        try {
            final V1PodList podList;
            if (Value.isEmpty(namespace)) {
                podList = api.listPodForAllNamespaces(
                        null, null, null, null, null, null, null, null, null, null, null);
            } else {
                podList = api.listNamespacedPod(
                        namespace, null, null, null, null, null, null, null, null, null, null, null);
            }
            final RowSet rowSet = loadRowSet(createMetaData(), podList);
            final UserStateTable table = new UserStateTable(getUserState(), null, getHttpRequest().getDate());
            table.toTableView(rowSet).addContentTo(html);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final V1PodList podList) {
        final String baseURI = PathU.toPath(getHttpRequest().getPathInfo());
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final V1Pod pod : podList.getItems()) {
            loadRow(rowSet, baseURI, pod);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData(FIELD_SELECT, Types.VARCHAR),
                new ColumnMetaData(FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(FIELD_NAMESPACE, Types.VARCHAR),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final String baseURI, final V1Pod pod) {
        final V1ObjectMeta metadata = pod.getMetadata();
        final String name = (metadata == null) ? null : metadata.getName();
        final String href = PathU.toDir(baseURI, name);

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, name, href));
        insertRow.setNextColumn((metadata == null) ? null : metadata.getName());
        insertRow.setNextColumn((metadata == null) ? null : metadata.getNamespace());
        rowSet.add(insertRow.getRow());
    }

    private static final String FIELD_SELECT = "select";
    private static final String FIELD_NAME = "name";
    private static final String FIELD_NAMESPACE = "namespace";
    private static final String TABLE_ID = "kubePodListType";
}
