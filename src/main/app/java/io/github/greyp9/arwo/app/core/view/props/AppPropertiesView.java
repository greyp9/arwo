package io.github.greyp9.arwo.app.core.view.props;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;

public class AppPropertiesView {
    private final String tableID;
    private final AppUserState userState;

    public AppPropertiesView(final String tableID, final AppUserState userState) {
        this.tableID = tableID;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(final Element html, final MetaFile metaFile,
                                           final Bundle bundle, final NameTypeValues properties) throws IOException {
        final RowSetMetaData metaData = createMetaData(tableID);
        final RowSet rowSet = createRowSet(metaData, metaFile, properties);
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(
                viewState, userState.getSubmitID(), App.CSS.TABLE, bundle, userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
        return null;
    }

    public static RowSetMetaData createMetaData(final String tableID) {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("name", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("value", Types.VARCHAR),  // i18n metadata
        };
        return new RowSetMetaData(tableID, columns);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private RowSet createRowSet(final RowSetMetaData metaData, final MetaFile metaFile,
                                final NameTypeValues properties) throws IOException {
        final Bundle bundle = userState.getBundle();
        final RowSet rowSet = new RowSet(metaData, null, null);
        if (metaFile != null) {
            final byte[] bytes = StreamU.read(metaFile.getBytes());
            properties.add(bundle.getString("propertiesView.CRC32"), Long.toHexString(CRCU.crc32(bytes)));
            properties.add(bundle.getString("propertiesView.MD5"), HexCodec.encode(HashU.md5(bytes)));
            properties.add(bundle.getString("propertiesView.SHA1"), HexCodec.encode(HashU.sha1(bytes)));
            properties.add(bundle.getString("propertiesView.SHA256"), HexCodec.encode(HashU.sha256(bytes)));
        }
        for (final NameTypeValue property : properties) {
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(bundle.getString(property.getName()));
            insertRow.setNextColumn(property.getValue());
            rowSet.add(insertRow.getRow());
        }
        return rowSet;
    }
}
