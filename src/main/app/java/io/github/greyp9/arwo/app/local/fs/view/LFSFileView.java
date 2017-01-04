package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.editor.AppFileEditView;
import io.github.greyp9.arwo.app.core.view.gz.AppTGZView;
import io.github.greyp9.arwo.app.core.view.hex.AppHexView;
import io.github.greyp9.arwo.app.core.view.rs.AppResultsView;
import io.github.greyp9.arwo.app.core.view.zip.AppZipView;
import io.github.greyp9.arwo.app.local.fs.action.LFSDeleteFile;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.app.local.fs.data.LFSFolder;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.gz.GZipCodec;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.result.io.ResultsPersister;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.text.filter.TextLineFilter;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Date;

@SuppressWarnings("PMD.ExcessiveImports")
public class LFSFileView extends LFSView {

    public LFSFileView(final LFSRequest request, final AppUserState userState, final File folderBase, final File file) {
        super(request, userState, folderBase, file);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final LFSRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        final String mode = request.getMode();
        final Bundle bundle = request.getBundle();
        final RowSetMetaData metaData = LFSFolder.createMetaData();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, request.getBundle(), locus);
        final MetaFile metaFile = getFileBytes(viewState);
        // resource interpret (UTF-8 versus Unicode)
        final String charset = userState.getCharset();
        // resource access (read versus write)
        final boolean isModeCreateF = App.Mode.CREATE_F.equals(mode);
        final boolean isModeCreateD = App.Mode.CREATE_D.equals(mode);
        final boolean isModeCreate = (isModeCreateF || isModeCreateD);
        final boolean isModeEdit = App.Mode.EDIT.equals(mode);
        final boolean isModeDelete = App.Mode.DELETE.equals(mode);
        // resource interpret (gzip deflated content expected)
        final boolean isModeGZ = App.Mode.VIEW_GZ.equals(mode);
        final boolean isModeZIP = App.Mode.VIEW_ZIP.equals(mode);
        final boolean isModeTGZ = App.Mode.VIEW_TGZ.equals(mode);
        // resource interpret (binary, view hex representation)
        final boolean isHex = App.Mode.VIEW_HEX.equals(mode);
        // resource interpret (webapp results)
        final boolean isResults = App.Mode.VIEW_R.equals(mode);
        // properties of cursor resource
        final boolean isProperties = PropertiesU.isBoolean(userState.getProperties(), App.Action.PROPERTIES);
        addFileProperties(html, metaFile);
        // dispose of request
        HttpResponse httpResponse;
        if (isModeCreate) {
            httpResponse = HttpResponseU.to302(".");  // go to containing folder
        } else if (isModeEdit) {
            httpResponse = new AppFileEditView(httpRequest, userState).addContentTo(html, metaFile, charset);
        } else if (isModeDelete) {
            final LFSDeleteFile action = new LFSDeleteFile(getRequest(), getFolderBase());
            userState.getDeferredActions().add(action);
            final String message = bundle.format("WebDAVFileView.file.delete.message", request.getPath());
            userState.getAlerts().add(new Alert(Alert.Severity.QUESTION, message, action.getActions()));
            httpResponse = HttpResponseU.to302(".");
        } else if (isProperties) {
            httpResponse = null;
        } else if (isModeZIP) {
            httpResponse = new AppZipView(httpRequest, userState).addContentTo(html, metaFile, bundle);
        } else if (isModeTGZ) {
            httpResponse = new AppTGZView(httpRequest, userState).addContentTo(html, metaFile, bundle);
        } else if (isHex) {
            httpResponse = new AppHexView(httpRequest, userState).addContentTo(html, metaFile, bundle);
        } else if (isResults) {
            httpResponse = new AppResultsView(httpRequest, userState).addContentTo(html, metaFile);
        } else {
            httpResponse = doGetFile(metaFile, charset, isModeGZ);
        }
        return httpResponse;
    }

    private MetaFile getFileBytes(final ViewState viewState) throws IOException {
        MetaFile metaFile;
        final LFSRequest request = getRequest();

        final LFSDataSource source = new LFSDataSource(request, getFolderBase());
        final ResourceCache cache = getUserState().getCache();
        final String path = request.getPath();
        // if disconnected, resource will only be fetched if no cached copy is available
        if (viewState.isConnected()) {
            metaFile = source.read(path);
            cache.putFile(path, metaFile);
        } else if (cache.containsFile(path)) {
            metaFile = cache.getFile(path);
        } else {
            metaFile = source.read(path);
            cache.putFile(path, metaFile);
        }
        return metaFile;
    }

    public final HttpResponse doGetFile(
            final MetaFile file, final String charset, final boolean isGZ) throws IOException {
        byte[] bytes = getBytes(file, isGZ);
        final String path = file.getMetaData().getPath();
        final String lastModified = HttpDateU.toHttpZ(new Date(file.getMetaData().getLastModified()));
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(Http.Header.LAST_MODIFIED, lastModified));
        final TextFilters textFilters = getUserState().getTextFilters();
        if ((textFilters.getIncludes().isEmpty()) && (textFilters.getExcludes().isEmpty())) {
            final Preferences preferences = new Preferences(getUserState().getConfig());
            final String mimeTypeOverride = getUserState().getProperties().getProperty(App.Action.MIME_TYPE);
            final String mimeTypePrefs = Value.defaultOnNull(mimeTypeOverride, preferences.getMIMEType(path));
            final String mimeType = Value.defaultOnEmpty(mimeTypePrefs, Http.Mime.TEXT_PLAIN_UTF8);
            headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, mimeType));
        } else if (UTF8Codec.Const.UTF16.equals(charset)) {
            headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF16));
            bytes = doTextFilter(bytes, charset);
        } else {
            headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8));
            bytes = doTextFilter(bytes, charset);
        }
        // optionally persist fetched results
        new ResultsPersister(getUserState().getResultsContext(getRequest().getHttpRequest())).write(bytes);
        // render for response
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(bytes));
    }

    private byte[] getBytes(final MetaFile file, final boolean isGZ) throws IOException {
        byte[] bytes = StreamU.read(file.getBytes());
        try {
            if (isGZ) {
                bytes = new GZipCodec().decode(bytes);
            }
        } catch (IOException e) {
            new ExceptionModel(getRequest().getAlerts()).service(e, Alert.Severity.WARN);
        }
        return bytes;
    }

    private byte[] doTextFilter(final byte[] bytes, final String charset) throws IOException {
        return new TextLineFilter(getUserState().getTextFilters()).doFilter(bytes, charset);
    }
}
