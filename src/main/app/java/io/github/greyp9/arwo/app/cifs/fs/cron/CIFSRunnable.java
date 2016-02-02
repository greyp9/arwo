package io.github.greyp9.arwo.app.cifs.fs.cron;

import io.github.greyp9.arwo.app.cifs.handler.CIFSHandlerGet;
import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.cron.core.CronParams;
import io.github.greyp9.arwo.core.cron.core.CronRunnable;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.http.Http;
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
import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

@SuppressWarnings("unused")  // reflection used to instantiate
public class CIFSRunnable extends CronRunnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public CIFSRunnable(final CronParams params) {
        super(params);
    }

    @Override
    public final void run() {
        logger.entering(getClass().getName(), CronRunnable.class.getName());
        final RowTyped row = getParams().getRow();
        final Date dateStart = new Date();
        row.update(Const.DATE_START, dateStart);
        // initialize
        final String server = ElementU.getAttribute(getParams().getElement(), App.Settings.SERVER);
        final String resource = ElementU.getAttribute(getParams().getElement(), App.Settings.RESOURCE);
        final String pathInfo = PathU.toPath("", App.Mode.VIEW, server + resource);
        final String filename = new File(resource).getName();
        // execute
        try {
            final Context context = AppNaming.lookupSubcontext(getParams().getContext());
            final AppState appState = (AppState) AppNaming.lookupQ(context, App.Naming.APP_STATE);
            final AppUserState userState = appState.getUserState(getParams().getPrincipal(), getParams().getDate());
            final Principal principal = userState.getPrincipal();
            final String resourceFull = getParams().getContext() + App.Servlet.CIFS + pathInfo;
            final HttpRequest httpRequest = getHttpRequest(resourceFull);
            final ServletHttpRequest httpRequest1 = getServletHttpRequest(httpRequest, pathInfo, principal);
            final CIFSHandlerGet handler = new CIFSHandlerGet(httpRequest1, userState);
            putHttpResponse(handler.doGetSafe(), filename, userState);
        } catch (IOException e) {
            logger.log(Level.SEVERE, e.getMessage(), e);
        } finally {
            row.update(Const.DURATION, DurationU.duration(dateStart, new Date()));
            row.update(Const.RESULT, 0);
            logger.exiting(getClass().getName(), CronRunnable.class.getName());
        }
    }

    private void putHttpResponse(
            final HttpResponse httpResponse, final String filename, final AppUserState userState) throws IOException {
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
        return new ServletHttpRequest(httpRequest, date, principal, context, App.Servlet.CIFS, pathInfo);
    }

    private HttpRequest getHttpRequest(final String resource) throws IOException {
        final CronParams params = getParams();
        final String filename = String.format("%s-%s-%s.results", params.getCronTab().getName(),
                params.getCronJob().getName(), DateX.toFilename(params.getDate()));
        final NameTypeValues headers = NameTypeValuesU.create(
                Http.Header.AUTHORIZATION, params.getAuthorization(),
                App.Header.RESULT, filename);
        return new HttpRequest(Http.Method.GET, resource, null, headers, null);
    }
}
