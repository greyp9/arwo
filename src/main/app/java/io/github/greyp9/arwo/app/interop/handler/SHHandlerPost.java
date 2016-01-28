package io.github.greyp9.arwo.app.interop.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.interop.action.SHAddFavorite;
import io.github.greyp9.arwo.app.interop.action.SHQueueCommand;
import io.github.greyp9.arwo.app.interop.action.SHSelectFavorite;
import io.github.greyp9.arwo.app.interop.core.SHRequest;
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

import java.io.IOException;

public class SHHandlerPost {
    private final SHRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public SHHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new SHRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final HttpResponse doPostSafe() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doPost();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to500(e.getMessage());
        }
        return httpResponse;
    }

    private HttpResponse doPost() throws IOException {
        // redirect location (identity by default)
        String location = httpRequest.getHttpRequest().getResource();
        // branch on content type of incoming request
        final String contentType = httpRequest.getHeader(Http.Header.CONTENT_TYPE);
        if (contentType == null) {
            doPostContentTypeUnknown(null);
        } else if (contentType.equalsIgnoreCase(Http.Mime.FORM_URL_ENCODED)) {
            location = doPostFormURLEncoded(location);
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
            final SubmitToken token, final NameTypeValues httpArguments, final String locationIn) throws IOException {
        String location = locationIn;
        final String message = request.getBundle().getString("alert.action.not.implemented");
        final String action = token.getAction();
        final String object = token.getObject();
        if (App.Action.COMMAND.equals(action)) {
            location = new SHQueueCommand(request).doAction(location, httpArguments);
        } else if (App.Action.FILESYSTEM.equals(action)) {
            location = PathU.toDir(httpRequest.getContextPath(), App.Cache.CIFS, App.Mode.VIEW, request.getServer());
        } else if (App.Action.TOGGLE.equals(action)) {
            PropertiesU.toggleBoolean(userState.getProperties(), Value.join(Http.Token.SLASH, App.Cache.WSH, object));
        } else if (App.Action.ADD_FAV.equals(action)) {
            new SHAddFavorite(request).doAction();
        } else if (App.Action.SELECT_FAV.equals(action)) {
            location = new SHSelectFavorite(request).doAction(token);
        } else {
            request.getAlerts().add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
        return location;
    }
}
