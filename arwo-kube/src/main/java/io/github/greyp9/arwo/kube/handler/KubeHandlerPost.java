package io.github.greyp9.arwo.kube.handler;

import io.github.greyp9.arwo.app.core.handler.AppHandlerPost;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.kube.xed.widget.XedWidgetKubeLog;

import java.io.IOException;
import java.util.Locale;
import java.util.Properties;

public class KubeHandlerPost extends AppHandlerPost {

    public KubeHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        super(httpRequest, userState);
    }

    @Override
    protected final String applySession(final SubmitToken token,
                                        final NameTypeValues httpArguments,
                                        final String locationIn) throws IOException {
        final AppUserState userState = getUserState();
        final XedFactory xedFactory = userState.getXedFactory();
        final Locale locale = userState.getLocale();
        final XedWidgetKubeLog xedWidgetKubeLog = new XedWidgetKubeLog(xedFactory, locale);
        final Properties properties = userState.getKube().getProperties();
        xedWidgetKubeLog.applyFrom(properties);
        xedWidgetKubeLog.applyFrom(httpArguments);  // incoming payload overwrites existing state
        xedWidgetKubeLog.applyTo(userState.getKube().getProperties());
        return locationIn;
    }
}
