package io.github.greyp9.arwo.app.dash.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.ClasspathU;
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
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.net.URLClassLoader;
import java.sql.Types;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.stream.Collectors;

@SuppressWarnings("FieldCanBeLocal")
public class ClasspathView {
    private final AppRequest request;
    private final AppUserState userState;
    private final URLClassLoader urlClassLoader;

    public ClasspathView(final ServletHttpRequest httpRequest, final AppUserState userState, final Class<?> c) {
        this.request = userState.getAppRequest(httpRequest);
        this.userState = userState;
        this.urlClassLoader = Value.as(c.getClassLoader(), URLClassLoader.class);
    }

    public final void addContent(final Element html) throws IOException {
        // in Java 11+, get classpath entries from system properties
        final Collection<File> files = (urlClassLoader == null) ? ClasspathU.getClassPath()
                : Arrays.stream(urlClassLoader.getURLs()).map(URLCodec::toFileUnchecked).collect(Collectors.toList());
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData, files);
        final Bundle bundle = request.getBundle();
        final Locus locus = request.getLocus();
        //final String submitID = request.getSubmitID();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData("path", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("mtime", Types.TIMESTAMP),  // i18n metadata
                new ColumnMetaData("size", Types.BIGINT),  // i18n metadata
        };
        return new RowSetMetaData("classloaderType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData,
                                final Collection<File> files) {
        final RowSet rowSet = new RowSet(metaData, null, null);
        if (!files.isEmpty()) {
            final File firstFile = files.iterator().next();
            final File folderBase = firstFile.getParentFile();
            for (final File file : files) {
                createRow(rowSet, file, folderBase);
            }
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final File file, final File folderBase) {
        final String path = file.getPath();
        final String relativePath = (folderBase == null) ? path : path.replace(folderBase.getPath(), "");
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(relativePath);
        insertRow.setNextColumn(new Date(file.lastModified()));
        insertRow.setNextColumn(file.length());
        rowSet.add(insertRow.getRow());
    }
}
