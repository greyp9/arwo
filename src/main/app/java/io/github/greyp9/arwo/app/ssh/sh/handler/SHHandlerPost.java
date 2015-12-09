package io.github.greyp9.arwo.app.ssh.sh.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;

import java.io.IOException;

public class SHHandlerPost {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SHHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doPost() throws IOException {
        httpRequest.getClass();
        userState.getClass();
        return HttpResponseU.to501();
    }
}
