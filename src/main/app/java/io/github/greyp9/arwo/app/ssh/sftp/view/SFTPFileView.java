package io.github.greyp9.arwo.app.ssh.sftp.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.editor.AppFileEditView;
import io.github.greyp9.arwo.app.core.view.gz.AppTGZView;
import io.github.greyp9.arwo.app.core.view.hex.AppHexView;
import io.github.greyp9.arwo.app.core.view.zip.AppZipView;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolder;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.gz.GZipCodec;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.text.TextFilters;
import io.github.greyp9.arwo.core.text.TextLineFilter;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;

@SuppressWarnings("PMD.ExcessiveImports")
public class SFTPFileView extends SFTPView {

    public SFTPFileView(
            final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        super(request, userState, resource);
    }

    @SuppressWarnings("PMD.AvoidFinalLocalVariable")  // PMD defect?
    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final SFTPRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        final String mode = request.getMode();
        final Bundle bundle = request.getBundle();
        final RowSetMetaData metaData = SFTPFolder.createMetaData();
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, request.getBundle(), locus);
        final MetaFile metaFile = getFileBytes(viewState);
        // resource interpret (UTF-8 versus Unicode)
        final Collection<String> utf16Modes = Arrays.asList("view16", "edit16");
        final boolean isUTF16 = utf16Modes.contains(mode);
        final String encoding = (isUTF16 ? UTF8Codec.Const.UTF16 : UTF8Codec.Const.UTF8);
        // resource access (read versus write)
        final Collection<String> editModes = Arrays.asList("edit", "edit16");
        final boolean isModeEdit = editModes.contains(mode);
        // resource interpret (gzip deflated content expected)
        final boolean isModeGZ = "viewGZ".equals(mode);
        final boolean isModeZIP = "viewZIP".equals(mode);
        final boolean isModeTGZ = "viewTGZ".equals(mode);
        // resource interpret (binary, view hex representation)
        final boolean isHex = "viewHex".equals(mode);
        // properties of cursor resource
        final boolean isProperties = PropertiesU.isBoolean(userState.getProperties(), App.Action.PROPERTIES);
        addFileProperties(html, metaFile);
        // dispose of request
        HttpResponse httpResponse;
        if (isModeEdit) {
            httpResponse = new AppFileEditView(httpRequest, userState).addContentTo(html, metaFile, encoding);
        } else if (isProperties) {
            httpResponse = null;
        } else if (isModeZIP) {
            httpResponse = new AppZipView(httpRequest, userState).addContentTo(html, metaFile, bundle);
        } else if (isModeTGZ) {
            httpResponse = new AppTGZView(httpRequest, userState).addContentTo(html, metaFile, bundle);
        } else if (isHex) {
            httpResponse = new AppHexView(httpRequest, userState).addContentTo(html, metaFile);
        } else {
            httpResponse = doGetFile(metaFile, encoding, isModeGZ);
        }
        return httpResponse;
    }

    private MetaFile getFileBytes(final ViewState viewState) throws IOException {
        MetaFile metaFile;
        final SFTPRequest request = getRequest();
        final SSHConnectionResource resource = getResource();
        final SFTPDataSource source = new SFTPDataSource(request, resource.getSSHConnection());
        final ResourceCache cache = getUserState().getCache();
        final String path = request.getPath();
        // if disconnected, resource will only be fetched if no cached copy is available
        if (viewState.isConnected()) {
            metaFile = source.read(path);
        } else if (cache.containsFile(path)) {
            metaFile = cache.getFile(path);
        } else {
            metaFile = source.read(path);
            cache.putFile(path, metaFile);
        }
        return metaFile;
    }

    public final HttpResponse doGetFile(
            final MetaFile file, final String encoding, final boolean isGZ) throws IOException {
        byte[] bytes = StreamU.read(file.getBytes());
        final String lastModified = HttpDateU.toHttpZ(new Date(file.getMetaData().getLastModified()));
        if (isGZ) {
            bytes = new GZipCodec().decode(bytes);
        }
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(Http.Header.LAST_MODIFIED, lastModified));
        final TextFilters textFilters = getUserState().getTextFilters();
        if ((textFilters.getIncludes().isEmpty()) && (textFilters.getExcludes().isEmpty())) {
            headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8));
        } else if (UTF8Codec.Const.UTF16.equals(encoding)) {
            headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF16));
            bytes = doTextFilter(bytes, encoding);
        } else {
            headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8));
            bytes = doTextFilter(bytes, encoding);
        }
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(bytes));
    }

    private byte[] doTextFilter(final byte[] bytes, final String encoding) throws IOException {
        return new TextLineFilter(getUserState().getTextFilters()).doFilter(bytes, encoding);
    }
}
