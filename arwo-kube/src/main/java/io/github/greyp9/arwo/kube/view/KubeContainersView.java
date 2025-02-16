package io.github.greyp9.arwo.kube.view;

import java.io.IOException;
import java.sql.Types;
import java.util.List;
import java.util.Objects;

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
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Container;
import io.kubernetes.client.openapi.models.V1Pod;
import io.kubernetes.client.openapi.models.V1PodSpec;
import org.w3c.dom.Element;

public class KubeContainersView extends KubeView {

    private final String namespace;
    private final String podName;

    public KubeContainersView(final ServletHttpRequest httpRequest, final AppUserState userState,
                              final KubeConnectionResource resource, final String namespace, final String podName) {
        super(httpRequest, userState, resource);
        this.namespace = namespace;
        this.podName = podName;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        try {
            final V1Pod v1Pod = api.readNamespacedPod(podName, namespace, null);
            final V1PodSpec v1PodSpec = Objects.requireNonNull(v1Pod.getSpec());
            final List<V1Container> containers = v1PodSpec.getContainers();
            final RowSet rowSet = loadRowSet(createMetaData(), containers);
            final UserStateTable table = new UserStateTable(getUserState(), null, getHttpRequest().getDate());
            table.toTableView(rowSet).addContentTo(html);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final List<V1Container> containers) {
        final String baseURI = getHttpRequest().getHttpRequest().getResource();
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final V1Container container : containers) {
            loadRow(rowSet, baseURI, container);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData(FIELD_SELECT, Types.VARCHAR),
                new ColumnMetaData(FIELD_NAME, Types.VARCHAR, true),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final String baseURI, final V1Container container) {
        final String name = container.getName();
        final String href = PathU.toDir(baseURI, name, CONTEXT_LOGS);

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, name, href));
        insertRow.setNextColumn(container.getName());
        rowSet.add(insertRow.getRow());
    }

    private static final String TABLE_ID = "kubeContainerListType";
}
