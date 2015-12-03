package io.github.greyp9.arwo.app.ssh.sftp.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.gz.AppTGZView;
import io.github.greyp9.arwo.app.core.view.zip.AppZipView;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.gz.GZipCodec;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.text.TextFilters;
import io.github.greyp9.arwo.core.text.TextLineFilter;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;

public class SFTPFileView extends SFTPView {

    public SFTPFileView(
            final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        super(request, userState, resource);
    }

    @SuppressWarnings("PMD.AvoidFinalLocalVariable")  // defect?
    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final SFTPRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = getUserState();
        final SSHConnectionResource resource = getResource();
        final String mode = request.getMode();
        final Bundle bundle = request.getBundle();

        final SFTPDataSource source = new SFTPDataSource(request, resource);
        final MetaFile metaFile = source.read(request.getPath());
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
        //final boolean isHex = "viewHex".equals(mode);
        // dispose of request
        HttpResponse httpResponse;
        if (isModeEdit) {
            httpResponse = doGetFile(metaFile, encoding, false);
        } else if (isModeZIP) {
            final AppZipView appZipView = new AppZipView(httpRequest, userState);
            httpResponse = appZipView.addContentTo(html, metaFile, bundle);
        } else if (isModeTGZ) {
            final AppTGZView appTGZView = new AppTGZView(httpRequest, userState);
            httpResponse = appTGZView.addContentTo(html, metaFile, bundle);
        } else {
            httpResponse = doGetFile(metaFile, encoding, isModeGZ);
        }
        return httpResponse;
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
