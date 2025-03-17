package io.github.greyp9.arwo.app.cron.handler;

import io.github.greyp9.arwo.app.core.handler.AppHandlerPost;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.IOException;

public class CronHandlerPost extends AppHandlerPost {

    public CronHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        super(httpRequest, userState);
    }

    @Override
    protected final String applySession(final SubmitToken token, final NameTypeValues httpArguments,
                                        final String locationIn) throws IOException {
        return locationIn;
    }
}
