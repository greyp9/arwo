package io.github.greyp9.arwo.app.webdav.fs.handler;

import io.github.greyp9.arwo.app.webdav.fs.core.DavFSRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.http.form.MimeHeader;
import io.github.greyp9.arwo.core.http.form.MimePart;
import io.github.greyp9.arwo.core.http.form.MultipartForm;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Properties;

public class DavFSHandlerPostMultipart {
    private final DavFSRequest request;
    private final ServletHttpRequest httpRequest;
    //private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public DavFSHandlerPostMultipart(final DavFSRequest request/*, final AppUserState userState*/) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        //this.userState = userState;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final String doPostMultipartFormData(final String location) throws IOException {
        final ByteArrayInputStream is = httpRequest.getHttpRequest().getEntity();
        final MultipartForm form = new MultipartForm(is);
        for (final Iterator<MimePart> mimePartIt = form.iterator(); mimePartIt.hasNext(); mimePartIt.getClass()) {
            final MimePart mimePart = mimePartIt.next();
            final Properties propertiesPart = new Properties();
            for (final Iterator<MimeHeader> headerIt = mimePart.iterator(); headerIt.hasNext(); headerIt.getClass()) {
                final MimeHeader mimeHeader = headerIt.next();
                mimeHeader.addTo(propertiesPart);
            }
            final String name = propertiesPart.getProperty(Const.CD_NAME);
            if (Const.UPLOAD_FILE.equals(name)) {
                doPostUploadFile(mimePart, propertiesPart);
            }
        }
        return location;
    }

    private void doPostUploadFile(final MimePart mimePart, final Properties properties) throws IOException {

        final String filename = properties.getProperty(Const.CD_FILENAME);





        if (filename.length() == 0) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SFTPHandlerPostMultipart.no.file", "")));
        } else {
            doPostUploadFile(mimePart, filename);

        }
    }

    private void doPostUploadFile(
            final MimePart mimePart, final String filename) throws IOException {
        final byte[] bytes = mimePart.getBody().toByteArray();
        alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                "SFTPHandlerPostMultipart.file.source", filename)));
        // put data to remote
        final FileX fileX = new FileX(Value.join("", request.getPath(), filename));
        //final DavFSDataSource source = new DavFSDataSource(request, request.getUserState().getUserRoot());
        //source.write(bytes, fileX.getFolder(), fileX.getFilename());
        // info alert
        final String hash = HexCodec.encode(HashU.md5(bytes));
        alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                "SFTPHandlerPostMultipart.file.target", fileX.getFolderSlash(), bytes.length, hash)));
    }

    private static class Const {
        private static final String CD_FILENAME = "Content-Disposition.filename";
        private static final String CD_NAME = "Content-Disposition.name";
        private static final String UPLOAD_FILE = "uploadFile";
    }
}
