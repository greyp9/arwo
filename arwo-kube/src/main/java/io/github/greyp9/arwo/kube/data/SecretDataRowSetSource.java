package io.github.greyp9.arwo.kube.data;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.models.V1Secret;

import java.sql.Types;
import java.util.Map;

public final class SecretDataRowSetSource implements RowSetSource {
    private final String baseURI;
    private final String endpoint;
    private final V1Secret v1Secret;

    public SecretDataRowSetSource(
            final String baseURI,
            final String endpoint,
            final V1Secret v1Secret) {
        this.baseURI = baseURI;
        this.endpoint = endpoint;
        this.v1Secret = v1Secret;
    }

    @Override
    public String getRowSetId() {
        return TABLE_ID;
    }

    public RowSet getRowSet() throws ApiException {
        final Map<String, byte[]> data = v1Secret.getData();
        return loadRowSet(createMetaData(), data);
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final Map<String, byte[]> data) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (Map.Entry<String, byte[]> entry : data.entrySet()) {
            loadRow(rowSet, entry);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(KubeSource.FIELD_SELECT, Types.DATALINK),
                new ColumnMetaData(KubeSource.FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(KubeSource.FIELD_DATA, Types.INTEGER),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final Map.Entry<String, byte[]> entry) {
        final String href = PathU.toPath(
                baseURI, endpoint, KubeSource.CONTEXT_SECRETS, v1Secret.getMetadata().getNamespace(),
                v1Secret.getMetadata().getName(), entry.getKey());
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(
                new TableViewLink(UTF16.SELECT, KubeSource.CONTEXT_SECRETS, href));
        insertRow.setNextColumn(entry.getKey());
        insertRow.setNextColumn(entry.getValue().length);
        rowSet.add(insertRow.getRow());
    }

    private static final String TABLE_ID = "kubeSecretDataType";
}
