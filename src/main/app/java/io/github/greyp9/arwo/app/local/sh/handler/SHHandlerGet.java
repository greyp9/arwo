package io.github.greyp9.arwo.app.local.sh.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;

import java.io.IOException;

public class SHHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SHHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        httpRequest.getClass();
        userState.getClass();
        return HttpResponseU.to501();
    }
}