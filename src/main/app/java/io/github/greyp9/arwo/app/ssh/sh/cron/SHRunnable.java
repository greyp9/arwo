package io.github.greyp9.arwo.app.ssh.sh.cron;

import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.sh.handler.SHHandlerPost;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.cron.core.CronParams;
import io.github.greyp9.arwo.core.cron.core.CronRunnable;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.type.RowTyped;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xml.ElementU;

import javax.naming.Context;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

@SuppressWarnings("unused")  // reflection used to instantiate
public class SHRunnable extends CronRunnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public SHRunnable(final CronParams params) {
        super(params);
    }

    @Override
    public final void run() {
        logger.entering(getClass().getName(), CronRunnable.class.getName());
        final RowTyped row = getParams().getRow();
        final Date dateStart = new Date();
        row.update(Const.DATE_START, dateStart);
        // initialize
        final String server = ElementU.getAttribute(getParams().getElement(), "server");
        final String command = ElementU.getAttribute(getParams().getElement(), "command");
        final String pathInfo = PathU.toDir("", server);
        // execute
        try {
            final Context context = AppNaming.lookupSubcontext(getParams().getContext());
            final AppState appState = (AppState) AppNaming.lookupQ(context, App.Naming.APP_STATE);
            final AppUserState userState = appState.getUserState(getParams().getPrincipal(), getParams().getDate());
            final Principal principal = userState.getPrincipal();
            final HttpRequest httpRequest = getHttpRequest(userState.getSubmitID(), pathInfo, command);
            final ServletHttpRequest httpRequest1 = getServletHttpRequest(httpRequest, pathInfo, principal);
            final SHHandlerPost handlerPost = new SHHandlerPost(httpRequest1, userState);
            putHttpResponse(handlerPost.doPost(), userState);
        } catch (IOException e) {
            logger.log(Level.SEVERE, e.getMessage(), e);
        } finally {
            row.update(Const.DURATION, DurationU.duration(dateStart, new Date()));
            row.update(Const.RESULT, 0);
            logger.exiting(getClass().getName(), CronRunnable.class.getName());
        }
    }

    private void putHttpResponse(final HttpResponse httpResponse, final AppUserState userState) throws IOException {
        if (!SystemU.isTrue()) {
            // persist invocation results
            final File file = getParams().getFile(userState.getUserRoot(), null);
            StreamU.writeMkdirs(file, UTF8Codec.toBytes(httpResponse.toString()));
        }
    }

    private ServletHttpRequest getServletHttpRequest(
            final HttpRequest httpRequest, final String pathInfo, final Principal principal) throws IOException {
        final Date date = getParams().getDate();
        final String context = getParams().getContext();
        return new ServletHttpRequest(httpRequest, date, principal, context, App.Servlet.SSH, pathInfo);
    }

    private HttpRequest getHttpRequest(
            final String submitID, final String pathInfo, final String command) throws IOException {
        final CronParams params = getParams();
        final NameTypeValues headers = NameTypeValuesU.create(
                Http.Header.AUTHORIZATION, params.getAuthorization(),
                Http.Header.CONTENT_TYPE, Http.Mime.FORM_URL_ENCODED,
                "X-Out", PathU.toDir("", "cron", params.getCronTab().getName(), params.getCronJob().getName()));
        final NameTypeValues query = NameTypeValuesU.create(
                "command.commandType.command", command, submitID, App.Actions.SUBMIT_COMMAND);
        final ByteArrayInputStream is = new ByteArrayInputStream(HttpArguments.toEntity(query));
        final String resource = String.format("%s%s%s", params.getContext(), App.Servlet.SSH, pathInfo);
        return new HttpRequest(Http.Method.POST, resource, null, headers, is);
    }
}
