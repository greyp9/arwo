package io.github.greyp9.arwo.s3.data;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.CommonPrefix;
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request;
import software.amazon.awssdk.services.s3.model.ListObjectsV2Response;
import software.amazon.awssdk.services.s3.model.S3Object;

import java.sql.Types;
import java.util.Date;

public final class S3RowSetSource implements RowSetSource {
    private final S3Client s3Client;
    private final String baseURI;
    private final String bucket;
    private final String prefix;

    public S3RowSetSource(final S3Client s3Client, final String baseURI, final String bucket, final String prefix) {
        this.s3Client = s3Client;
        this.baseURI = baseURI;
        this.bucket = bucket;
        this.prefix = prefix;
    }

    @Override
    public String getRowSetId() {
        return "";
    }

    @Override
    public RowSet getRowSet() {
        final ListObjectsV2Request listObjectsRequest = ListObjectsV2Request.builder()
                .bucket(bucket).prefix(prefix).delimiter("/").build();
        final ListObjectsV2Response listObjectsResponse = s3Client.listObjectsV2(listObjectsRequest);
        return loadRowSet(createMetaData(""), listObjectsResponse);
    }

    private RowSet loadRowSet(final RowSetMetaData metaData, final ListObjectsV2Response listObjects) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (CommonPrefix commonPrefix : listObjects.commonPrefixes()) {
            loadRow(rowSet, commonPrefix);
        }
        for (S3Object s3Object : listObjects.contents()) {
            if (!s3Object.key().equals(prefix)) {
                loadRow(rowSet, s3Object);
            }
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData(final String tableId) {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData("select", Types.DATALINK),
                new ColumnMetaData("name", Types.VARCHAR, true),
                new ColumnMetaData("timestamp", Types.TIMESTAMP),
                new ColumnMetaData("etag", Types.VARCHAR),
                new ColumnMetaData("extension", Types.VARCHAR),
                new ColumnMetaData("length", Types.INTEGER),
        };
        return new RowSetMetaData(tableId, columns);
    }

    private void loadRow(final RowSet rowSet, final CommonPrefix commonPrefix) {
        final String href = PathU.toPath(baseURI, commonPrefix.prefix());
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, App.Action.SELECT, href));
        insertRow.setNextColumn(commonPrefix.prefix().substring(prefix.length()));
        rowSet.add(insertRow.getRow());
    }

    private void loadRow(final RowSet rowSet, final S3Object s3Object) {
        final String href = PathU.toPath(baseURI, s3Object.key());
        final String extension = new FileX(s3Object.key()).getExtension();
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, App.Action.SELECT, href));
        insertRow.setNextColumn(s3Object.key().substring(prefix.length()));
        insertRow.setNextColumn(new Date(s3Object.lastModified().toEpochMilli()));
        insertRow.setNextColumn(s3Object.eTag());
        insertRow.setNextColumn(extension);
        insertRow.setNextColumn(s3Object.size());
        rowSet.add(insertRow.getRow());
    }
}
