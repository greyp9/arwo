package io.github.greyp9.arwo.app.cron.handler;

import io.github.greyp9.arwo.app.core.handler.AppHandlerPost;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;

public class CronHandlerPost extends AppHandlerPost {

    public CronHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        super(httpRequest, userState);
    }
}
