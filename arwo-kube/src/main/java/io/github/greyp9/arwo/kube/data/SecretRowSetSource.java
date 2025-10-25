package io.github.greyp9.arwo.kube.data;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.cache.V1SecretCacher;
import io.github.greyp9.arwo.kube.connection.KubeConnection;
import io.github.greyp9.arwo.kube.core.KubeU;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.openapi.models.V1Secret;
import io.kubernetes.client.openapi.models.V1SecretList;

import java.sql.Types;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.Map;

public final class SecretRowSetSource implements RowSetSource {
    private final V1SecretCacher cacher;  // cache V1Secret objects
    private final KubeConnection connection;
    private final String namespace;
    private final String baseURI;
    private final String endpoint;

    public SecretRowSetSource(
            final V1SecretCacher cacher,
            final KubeConnection connection,
            final String namespace,
            final String baseURI,
            final String endpoint) {
        this.cacher = cacher;
        this.connection = connection;
        this.namespace = namespace;
        this.baseURI = baseURI;
        this.endpoint = endpoint;
    }

    @Override
    public String getRowSetId() {
        return TABLE_ID;
    }

    public RowSet getRowSet() throws ApiException {
        final V1SecretList secretList;
        final CoreV1Api api = connection.getCoreV1Api();
        final Date date = new Date();
        if (Value.isEmpty(namespace)) {
            secretList = api.listSecretForAllNamespaces(
                    null, null, null, null, null, null, null, null, null, null, null);
        } else {
            secretList = api.listNamespacedSecret(
                    namespace, null, null, null, null, null, null, null, null, null, null, null);
        }
        connection.update(date);
        return loadRowSet(createMetaData(), secretList);
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final V1SecretList secretList) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final V1Secret secret : secretList.getItems()) {
            cacher.cache(secret);
            loadRow(rowSet, secret);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(KubeSource.FIELD_SELECT, Types.DATALINK),
                new ColumnMetaData(KubeSource.FIELD_NAMESPACE, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(KubeSource.FIELD_TYPE, Types.VARCHAR),
                new ColumnMetaData(KubeSource.FIELD_DATA, Types.INTEGER),
                new ColumnMetaData(KubeSource.FIELD_CREATED, Types.TIMESTAMP),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final V1Secret secret) {
        final V1ObjectMeta metadata = secret.getMetadata();
        final Map<String, byte[]> data = secret.getData();
        final String namespaceRow = (metadata == null) ? null : metadata.getNamespace();
        final String name = (metadata == null) ? null : metadata.getName();
        final OffsetDateTime creationTimestamp = (metadata == null) ? null : metadata.getCreationTimestamp();
        final String hrefDescribe = PathU.toDir(
                baseURI, endpoint, KubeSource.CONTEXT_SECRETS, namespaceRow, name);

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(
                new TableViewLink(UTF16.SELECT, KubeSource.CONTEXT_NODES, hrefDescribe));
        insertRow.setNextColumn(namespaceRow);
        insertRow.setNextColumn(name);
        insertRow.setNextColumn(secret.getType());
        insertRow.setNextColumn((data == null) ? 0 : data.size());
        insertRow.setNextColumn((metadata == null) ? null : KubeU.toDate(creationTimestamp));
        rowSet.add(insertRow.getRow());
    }

    private static final String TABLE_ID = "kubeSecretListType";
}
