package io.github.greyp9.arwo.app.local.sh.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh.action.SHAddFavorite;
import io.github.greyp9.arwo.app.local.sh.action.SHQueueCommand;
import io.github.greyp9.arwo.app.local.sh.action.SHSelectFavorite;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
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

    public SHHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new SHRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doPost() throws IOException {
        // redirect location (identity by default)
        String location = httpRequest.getHttpRequest().getResource();
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
        return HttpResponseU.to302(location);
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
            location = new SHQueueCommand(request).doAction(httpArguments);
        } else if (App.Action.FILESYSTEM.equals(action)) {
            location = PathU.toDir(httpRequest.getContextPath(), "lfs", App.Mode.VIEW);
        } else if (App.Action.TOGGLE.equals(action)) {
            PropertiesU.toggleBoolean(userState.getProperties(), Value.join("/", "lsh", object));
        } else if (App.Action.ADD_FAV.equals(action)) {
            new SHAddFavorite(request).doAction();
        } else if (App.Action.SELECT_FAV.equals(action)) {
            location = new SHSelectFavorite(request).doAction(token);
        } else {
            request.getAlerts().add(new Alert(Alert.Severity.WARN, message, token.toString(), null));
        }
        return location;
    }
}
