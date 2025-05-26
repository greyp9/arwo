package io.github.greyp9.arwo.kube.data;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.cell.TableViewLinks;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.core.KubeU;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1ContainerStatus;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.openapi.models.V1Pod;
import io.kubernetes.client.openapi.models.V1PodList;
import io.kubernetes.client.openapi.models.V1PodSpec;
import io.kubernetes.client.openapi.models.V1PodStatus;

import java.sql.Types;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public final class PodRowSetSource implements RowSetSource {
    private final CoreV1Api api;
    private final String nodeId;
    private final String namespace;
    private final String baseURI;
    private final String endpoint;

    public PodRowSetSource(
            final CoreV1Api api,
            final String nodeId,
            final String namespace,
            final String baseURI,
            final String endpoint) {
        this.api = api;
        this.nodeId = nodeId;
        this.namespace = namespace;
        this.baseURI = baseURI;
        this.endpoint = endpoint;
    }

    @Override
    public String getRowSetId() {
        return TABLE_ID;
    }

    public RowSet getRowSet() throws ApiException {
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
        return loadRowSet(createMetaData(), podList);
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final V1PodList podList) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final V1Pod pod : podList.getItems()) {
            loadRow(rowSet, pod);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(KubeSource.FIELD_SELECT, Types.DATALINK),
                new ColumnMetaData(KubeSource.FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(KubeSource.FIELD_NAMESPACE, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_READY, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_STATUS, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_POD_IP, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_HOST_IP, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_CREATED, Types.TIMESTAMP),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final V1Pod pod) {
        final V1ObjectMeta metadata = pod.getMetadata();
        final V1PodSpec spec = pod.getSpec();
        final V1PodStatus status = pod.getStatus();
        final List<V1ContainerStatus> containerStatuses = (status == null)
                ? new ArrayList<>() : status.getContainerStatuses();
        final List<V1ContainerStatus> containerStatuses1 = Optional.ofNullable(containerStatuses)
                .orElse(new ArrayList<>());
        final long countReady = containerStatuses1.stream().filter(V1ContainerStatus::getReady).count();
        final String ready = String.format("%d/%d", countReady, containerStatuses1.size());
        final String podIP = (status == null) ? null : status.getPodIP();
        final String hostIP = (status == null) ? null : status.getHostIP();
        final String statusText = (status == null) ? null : status.getPhase();
        final String namespacePod = (metadata == null) ? null : metadata.getNamespace();
        final String name = (metadata == null) ? null : metadata.getName();
        final String nodeName = (spec == null) ? null : spec.getNodeName();
        final OffsetDateTime creationTimestamp = (metadata == null) ? null : metadata.getCreationTimestamp();
        final String hrefContainers = PathU.toDir(
                baseURI, endpoint, KubeSource.CONTEXT_PODS, namespacePod, name, KubeSource.CONTEXT_CONTAINERS);
        final String hrefDescribe = PathU.toDir(
                baseURI, endpoint, KubeSource.CONTEXT_PODS, namespacePod, name, KubeSource.CONTEXT_DESCRIBE);
        final String hrefNode = PathU.toDir(
                baseURI, endpoint, KubeSource.CONTEXT_NODES, nodeName, KubeSource.CONTEXT_PODS);

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLinks(Arrays.asList(
                new TableViewLink(UTF16.SELECT, KubeSource.CONTEXT_CONTAINERS, hrefContainers),
                new TableViewLink(UTF16.SELECT, KubeSource.CONTEXT_DESCRIBE, hrefDescribe),
                new TableViewLink(UTF16.SELECT, KubeSource.CONTEXT_NODES, hrefNode))));
        insertRow.setNextColumn((metadata == null) ? null : metadata.getName());
        insertRow.setNextColumn((metadata == null) ? null : namespacePod);
        insertRow.setNextColumn(ready);
        insertRow.setNextColumn(statusText);
        insertRow.setNextColumn((metadata == null) ? null : podIP);
        insertRow.setNextColumn((metadata == null) ? null : hostIP);
        insertRow.setNextColumn((metadata == null) ? null : KubeU.toDate(creationTimestamp));
        rowSet.add(insertRow.getRow());
    }

    private static final String FIELD_SELECTOR_NODE_NAME = "spec.nodeName=%s";

    private static final String TABLE_ID = "kubePodListType";
}
