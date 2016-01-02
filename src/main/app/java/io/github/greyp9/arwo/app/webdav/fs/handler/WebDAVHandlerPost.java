package io.github.greyp9.arwo.app.webdav.fs.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.webdav.action.WebDAVAddFavorite;
import io.github.greyp9.arwo.app.webdav.action.WebDAVCreateFile;
import io.github.greyp9.arwo.app.webdav.action.WebDAVSelectFavorite;
import io.github.greyp9.arwo.app.webdav.action.WebDAVUpdateFile;
import io.github.greyp9.arwo.app.webdav.connection.WebDAVConnectionFactory;
import io.github.greyp9.arwo.app.webdav.connection.WebDAVConnectionResource;
import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.submit.SubmitTokenU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.WebDAVConnection;

import java.io.IOException;

public class WebDAVHandlerPost {
    private final WebDAVRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public WebDAVHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new WebDAVRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final HttpResponse doPost() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doPost2();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to302(httpRequest.getHttpRequest().getResource());
        }
        return httpResponse;
    }

    private HttpResponse doPost2() throws IOException {
        // redirect location (identity by default)
        String location = httpRequest.getHttpRequest().getResource();
        // branch on content type of incoming request
        final String contentType = httpRequest.getHeader(Http.Header.CONTENT_TYPE);
        if (contentType == null) {
            doPostContentTypeUnknown(null);
        } else if (contentType.equalsIgnoreCase(Http.Mime.FORM_URL_ENCODED)) {
            location = doPostFormURLEncoded(location);
        } else if (contentType.startsWith(Http.Mime.FORM_MULTIPART)) {
            final WebDAVHandlerPostMultipart handler = new WebDAVHandlerPostMultipart(request, userState);
            location = handler.doPostMultipartFormData(location);
        } else {
            doPostContentTypeUnknown(contentType);
        }
        // redirect to clean up client POST state
        return HttpResponseU.to302(location);
    }

    private void doPostContentTypeUnknown(final String contentType) throws IOException {
        alerts.add(new Alert(Alert.Severity.ERR, bundle.format("SFTPHandlerPost.type.unknown", contentType)));
    }

    private String doPostFormURLEncoded(final String locationIn) throws IOException {
        String location = locationIn;
        final byte[] entity = StreamU.read(httpRequest.getHttpRequest().getEntity());
        final NameTypeValues httpArguments = HttpArguments.toArguments(entity);
        for (final NameTypeValue httpArgument : httpArguments) {
            if (userState.getSubmitID().equals(httpArgument.getName())) {
                final SubmitToken token = SubmitTokenU.fromString(httpArgument.getValueS());
                if (token != null) {
                    location = applySubmit(token, httpArguments, location);
                }
            }
        }
        return location;
    }

    private String applySubmit(
            final SubmitToken token, final NameTypeValues httpArguments, final String locationIn) throws IOException {
        String location = locationIn;
        final String subject = token.getSubject();
        if (App.Target.USER_STATE.equals(subject)) {
            location = userState.applyPost(token, httpArguments, httpRequest);
        } else if (App.Target.VIEW_STATE.equals(subject)) {
            userState.getViewStates().apply(token, httpArguments, request.getBundle(), request.getAlerts());
        } else if (App.Target.SESSION.equals(subject)) {
            location = applySession(token, httpArguments, location);
        }
        return location;
    }

    private String applySession(
            final SubmitToken token, final NameTypeValues httpArguments, final String locationIn)
            throws IOException {
        String location = locationIn;
        final String message = request.getBundle().getString("alert.action.not.implemented");
        final String action = token.getAction();
        final String object = token.getObject();
        if (App.Action.FILE_CREATE.equals(action)) {
            new WebDAVCreateFile(request, getConnection()).apply(httpArguments);
        } else if (App.Action.FILE_UPDATE.equals(action)) {
            new WebDAVUpdateFile(request, getConnection()).apply(httpArguments);
        } else if (App.Action.COMMAND.equals(action)) {
            location = PathU.toDir(httpRequest.getContextPath(), "webdav", request.getServer());
        } else if (App.Action.TOGGLE.equals(action)) {
            PropertiesU.toggleBoolean(userState.getProperties(), Value.join("/", "webdav", object));
        } else if (App.Action.ADD_FAV.equals(action)) {
            new WebDAVAddFavorite(request).doAction();
        } else if (App.Action.SELECT_FAV.equals(action)) {
            location = new WebDAVSelectFavorite(request).doAction(token);
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString(), null));
        }
        return location;
    }

    private WebDAVConnection getConnection() throws IOException {
        final WebDAVConnectionFactory factory = new WebDAVConnectionFactory(httpRequest, userState, bundle, alerts);
        final WebDAVConnectionResource resource = (WebDAVConnectionResource)
                userState.getWebDAV().getCache().getResource(request.getServer(), factory);
        return resource.getConnection();
    }
}
