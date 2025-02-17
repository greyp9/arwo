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
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.core.KubeU;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.openapi.models.V1Pod;
import io.kubernetes.client.openapi.models.V1PodList;
import io.kubernetes.client.openapi.models.V1PodSpec;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.time.OffsetDateTime;
import java.util.Arrays;

@SuppressWarnings("PMD.ExcessiveImports")
public class KubePodsView extends KubeView {
    private final String nodeId;

    @SuppressWarnings("WeakerAccess")
    public KubePodsView(final ServletHttpRequest httpRequest,
                        final AppUserState userState,
                        final KubeConnectionResource resource,
                        final String nodeId) {
        super(httpRequest, userState, resource);
        this.nodeId = nodeId;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        // https://javadoc.io/doc/io.kubernetes/client-java-api/19.0.1/io/kubernetes/client/openapi/apis/CoreV1Api.html
        final String namespace = resource.getNamespace();
        try {
            final V1PodList podList;
            if (nodeId != null) {
                final String fieldSelector = String.format(FIELD_SELECTOR_NODE_NAME, nodeId);
                podList = api.listPodForAllNamespaces(
                        null, null, fieldSelector, null, null, null, null, null, null, null, null);
            } else if (Value.isEmpty(namespace)) {
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
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getResponseBody()));
        }
        return null;
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final V1PodList podList) {
        final String baseURI = getHttpRequest().getBaseURI();
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final V1Pod pod : podList.getItems()) {
            loadRow(rowSet, baseURI, pod);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData(FIELD_SELECT, Types.DATALINK),
                new ColumnMetaData(FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(FIELD_NAMESPACE, Types.VARCHAR),
                new ColumnMetaData(FIELD_CREATED, Types.TIMESTAMP),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final String baseURI, final V1Pod pod) {
        final String endpoint = getResource().getName();
        final V1ObjectMeta metadata = pod.getMetadata();
        final V1PodSpec spec = pod.getSpec();
        final String namespace = (metadata == null) ? null : metadata.getNamespace();
        final String name = (metadata == null) ? null : metadata.getName();
        final String nodeName = (spec == null) ? null : spec.getNodeName();
        final OffsetDateTime creationTimestamp = (metadata == null) ? null : metadata.getCreationTimestamp();
        final String hrefContainers = PathU.toDir(baseURI, endpoint, CONTEXT_PODS, namespace, name, CONTEXT_CONTAINERS);
        final String hrefDescribe = PathU.toDir(baseURI, endpoint, CONTEXT_PODS, namespace, name, CONTEXT_DESCRIBE);
        final String hrefNode = PathU.toDir(baseURI, endpoint, CONTEXT_NODES, nodeName, CONTEXT_PODS);

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLinks(Arrays.asList(
                new TableViewLink(UTF16.SELECT, CONTEXT_CONTAINERS, hrefContainers),
                new TableViewLink(UTF16.SELECT, CONTEXT_DESCRIBE, hrefDescribe),
                new TableViewLink(UTF16.SELECT, CONTEXT_NODES, hrefNode))));
        insertRow.setNextColumn((metadata == null) ? null : metadata.getName());
        insertRow.setNextColumn((metadata == null) ? null : namespace);
        insertRow.setNextColumn((metadata == null) ? null : KubeU.toDate(creationTimestamp));
        rowSet.add(insertRow.getRow());
    }

    private static final String FIELD_SELECTOR_NODE_NAME = "spec.nodeName=%s";

    private static final String TABLE_ID = "kubePodListType";
}
