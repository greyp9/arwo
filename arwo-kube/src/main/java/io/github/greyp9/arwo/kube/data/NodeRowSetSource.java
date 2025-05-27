package io.github.greyp9.arwo.kube.data;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.number.NumberScale;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.cell.TableViewLinks;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.kube.connection.KubeConnection;
import io.github.greyp9.arwo.kube.core.KubeU;
import io.kubernetes.client.custom.Quantity;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Node;
import io.kubernetes.client.openapi.models.V1NodeCondition;
import io.kubernetes.client.openapi.models.V1NodeList;
import io.kubernetes.client.openapi.models.V1NodeSpec;
import io.kubernetes.client.openapi.models.V1NodeStatus;
import io.kubernetes.client.openapi.models.V1NodeSystemInfo;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.openapi.models.V1Taint;

import java.sql.Types;
import java.time.OffsetDateTime;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

public final class NodeRowSetSource implements RowSetSource {
    private final KubeConnection connection;
    private final String baseURI;
    private final String endpoint;

    public NodeRowSetSource(
            final KubeConnection connection,
            final String baseURI,
            final String endpoint) {
        this.connection = connection;
        this.baseURI = baseURI;
        this.endpoint = endpoint;
    }

    @Override
    public String getRowSetId() {
        return TABLE_ID;
    }

    public RowSet getRowSet() throws ApiException {
        final CoreV1Api api = connection.getCoreV1Api();
        final Date date = new Date();
        final V1NodeList nodeList = api.listNode(
                null, null, null, null, null, null, null, null, null, null, null);
        connection.update(date);
        return loadRowSet(createMetaData(), nodeList);
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final V1NodeList v1NodeList) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final V1Node v1Node : v1NodeList.getItems()) {
            loadRow(rowSet, v1Node);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData(KubeSource.FIELD_SELECT, Types.DATALINK),
                new ColumnMetaData(KubeSource.FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(KubeSource.FIELD_STATUS, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_TAINTS, Types.INTEGER),
                new ColumnMetaData(KubeSource.FIELD_FINALIZERS, Types.INTEGER),
                new ColumnMetaData(KubeSource.FIELD_VERSION, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_CPU, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_STORAGE, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_MEMORY, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_CREATED, Types.TIMESTAMP),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final V1Node v1Node) {
        final V1ObjectMeta metadata = v1Node.getMetadata();
        final V1NodeSpec spec = v1Node.getSpec();
        final V1NodeStatus status = v1Node.getStatus();
        final List<String> finalizers = (metadata == null) ? null : metadata.getFinalizers();
        final List<V1Taint> taints = (spec == null) ? null : spec.getTaints();
        final List<V1NodeCondition> conditions = (status == null) ? null : status.getConditions();
        final V1NodeCondition condition = (conditions == null)
                ? null : conditions.stream().reduce((f, s) -> s).orElse(null);
        final V1NodeSystemInfo nodeInfo = (status == null) ? null : status.getNodeInfo();
        final Map<String, Quantity> allocatable = Optional.ofNullable((status == null)
                ? null : status.getAllocatable()).orElse(new TreeMap<>());
        final Map<String, Quantity> capacity = Optional.ofNullable((status == null)
                ? null : status.getCapacity()).orElse(new TreeMap<>());
        final Quantity cpuA = allocatable.get(KubeSource.STATUS_CPU);
        final Quantity cpuC = capacity.get(KubeSource.STATUS_CPU);
        final String cpu = String.format("%.2f (%.2f)", cpuA.getNumber(), cpuC.getNumber());
        final Quantity storageA = allocatable.get(KubeSource.STATUS_STORAGE);
        final Quantity storageC = capacity.get(KubeSource.STATUS_STORAGE);
        final String storage = String.format("%s (%s)",
                NumberScale.toString(storageA.getNumber().longValue()),
                NumberScale.toString(storageC.getNumber().longValue()));
        final Quantity memoryA = allocatable.get(KubeSource.STATUS_MEMORY);
        final Quantity memoryC = capacity.get(KubeSource.STATUS_MEMORY);
        final String memory = String.format("%s (%s)",
                NumberScale.toString(memoryA.getNumber().longValue()),
                NumberScale.toString(memoryC.getNumber().longValue()));
        final String name = (metadata == null) ? null : metadata.getName();
        final OffsetDateTime creationTimestamp = (metadata == null) ? null : metadata.getCreationTimestamp();
        final String hrefPods = PathU.toDir(
                baseURI, endpoint, KubeSource.CONTEXT_NODES, name, KubeSource.CONTEXT_PODS);
        final String hrefDescribe = PathU.toDir(
                baseURI, endpoint, KubeSource.CONTEXT_NODES, name, KubeSource.CONTEXT_DESCRIBE);

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLinks(Arrays.asList(
                new TableViewLink(UTF16.SELECT, KubeSource.CONTEXT_PODS, hrefPods),
                new TableViewLink(UTF16.SELECT, KubeSource.CONTEXT_DESCRIBE, hrefDescribe))));
        insertRow.setNextColumn((metadata == null) ? null : metadata.getName());
        insertRow.setNextColumn((condition == null) ? null : condition.getType());
        insertRow.setNextColumn((taints == null) ? 0 : taints.size());
        insertRow.setNextColumn((finalizers == null) ? 0 : finalizers.size());
        insertRow.setNextColumn((nodeInfo == null) ? null : nodeInfo.getKubeletVersion());
        insertRow.setNextColumn(cpu);
        insertRow.setNextColumn(storage);
        insertRow.setNextColumn(memory);
        insertRow.setNextColumn((metadata == null) ? null : KubeU.toDate(creationTimestamp));
        rowSet.add(insertRow.getRow());
    }

    private static final String TABLE_ID = "kubeNodeListType";
}
