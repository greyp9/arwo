package io.github.greyp9.arwo.s3.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.s3.core.S3;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collection;

public final class EndpointRowSet implements RowSetSource {
    private final ServletHttpRequest httpRequest;
    //private final AppUserState userState;
    private final XedCursor cursor;

    public EndpointRowSet(final ServletHttpRequest httpRequest, final AppUserState userState) throws IOException {
        this.httpRequest = httpRequest;
        //this.userState = userState;
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.SETTINGS).getXed();
        final Element element = xed.getXPather().getElement(S3.XPATH_ENDPOINTS);
        this.cursor = (element == null) ? null : new XedNav(xed).find(element);
    }

    @Override
    public String getRowSetId() {
        return S3.ENDPOINTS_TYPE;
    }

    @Override
    public RowSet getRowSet() {
        final String baseURI = PathU.toPath(httpRequest.getBaseURI());
        final RowSet rowSet = new RowSet(createMetaData(), null, null);
        final TypeInstance s3Endpoint = cursor.getChildInstance(S3.TYPE_INSTANCE_NAME);
        final Collection<Element> children = cursor.getChildren(s3Endpoint);
        for (Element child : children) {
            final XedCursor cursorChild = new XedNav(cursor.getXed()).find(child);
            loadRow(rowSet, baseURI, cursorChild);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(App.Action.SELECT, Types.DATALINK),
                new ColumnMetaData(App.Settings.NAME, Types.VARCHAR, true),
                new ColumnMetaData(App.Settings.COMMENT, Types.VARCHAR),
        };
        return new RowSetMetaData(S3.ENDPOINTS_TYPE, columns);
    }

    private void loadRow(final RowSet rowSet, final String baseURI, final XedCursor cursor1) {
        final boolean isEnabled = TypeU.toBooleanP(cursor1.getValue(cursor1.getChildInstance(App.Settings.ENABLED)));
        if (isEnabled) {
            final String name = cursor1.getValue(cursor1.getChildInstance(App.Settings.NAME));
            final String comment = cursor1.getValue(cursor1.getChildInstance(App.Settings.COMMENT));
            final String uri = PathU.toDir(baseURI, name);
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, name, uri));
            insertRow.setNextColumn(name);
            insertRow.setNextColumn(comment);
            rowSet.add(insertRow.getRow());
        }
    }
}
