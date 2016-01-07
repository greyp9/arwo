package io.github.greyp9.arwo.app.cifs.handler;

import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.form.MimeHeader;
import io.github.greyp9.arwo.core.http.form.MimePart;
import io.github.greyp9.arwo.core.http.form.MultipartForm;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Properties;

public class CIFSHandlerPostMultipart {
    private final CIFSRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public CIFSHandlerPostMultipart(final CIFSRequest request, final AppUserState userState) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;
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
            //if (Const.UPLOAD_FILE.equals(name)) {
            //    doPostUploadFile(mimePart, propertiesPart);
            //}
        }
        return location;
    }

    private static class Const {
        private static final String CD_FILENAME = "Content-Disposition.filename";
        private static final String CD_NAME = "Content-Disposition.name";
        private static final String UPLOAD_FILE = "uploadFile";
    }
}
