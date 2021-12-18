package io.github.greyp9.arwo.app.vis.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.submit.SubmitTokenU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class VisualizationHandlerPost {
    private final VisualizationRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public VisualizationHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new VisualizationRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.bundle = request.getAppRequest().getBundle();
        this.alerts = request.getAppRequest().getAlerts();
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

    private void doPostContentTypeUnknown(final String contentType) {
        alerts.add(new Alert(Alert.Severity.ERR, bundle.format("ArwoHandlerPost.type.unknown", contentType)));
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
            location = userState.applyPost(token, httpArguments, httpRequest, httpRequest.getContextPath());
        } else if (App.Target.VIEW_STATE.equals(subject)) {
            userState.getViewStates().apply(token, httpArguments, bundle, alerts);
        } else if (App.Target.SESSION.equals(subject)) {
            location = applySession(token, httpArguments, location);
        }
        return location;
    }

    private String applySession(
            final SubmitToken token, final NameTypeValues httpArguments, final String locationIn) {
        //noinspection ResultOfMethodCallIgnored
        httpArguments.getClass();
        final String baseURI = httpRequest.getBaseURI();
        final String context = request.getContext();
        final String metric = request.getMetric();
        final String mode = request.getMode();
        final String page = request.getPage();
        final String scale = request.getScale();
        // operate on input token
        String location = locationIn;
        final String message = bundle.getString("alert.action.not.implemented");
        final String action = token.getAction();
        final String object = token.getObject();
        if (App.Action.SELECT.equals(action) && App.Mode.VIEW_HTML.equals(object)) {
            location = PathU.toDir(baseURI, context, metric, Html.HTML, page, scale);
        } else if (App.Action.SELECT.equals(action) && App.Mode.VIEW_TEXT.equals(object)) {
            location = PathU.toDir(baseURI, context, metric, Html.TEXT, page, scale);
        } else if (App.Action.SELECT.equals(action) && App.Mode.VIEW.equals(object)) {
            location = PathU.toDir(baseURI, context, metric, Html.FILE);
        } else if (App.Action.SELECT.equals(action) && "log1.5".equals(object)) {
            location = PathU.toDir(baseURI, context, metric, mode, page, "1.5");
        } else if (App.Action.SELECT.equals(action) && "log2".equals(object)) {
            location = PathU.toDir(baseURI, context, metric, mode, page, Integer.toString(2));
        } else if (App.Action.SELECT.equals(action) && "log3".equals(object)) {
            final int log = 3;
            location = PathU.toDir(baseURI, context, metric, mode, page, Integer.toString(log));
        } else if (App.Action.SELECT.equals(action) && "navMetricPrev".equals(object)) {
            location = PathU.toDir(baseURI, context, iterateMetric(context, metric, -1), mode, page, scale);
        } else if (App.Action.SELECT.equals(action) && "navMetricNext".equals(object)) {
            location = PathU.toDir(baseURI, context, iterateMetric(context, metric, 1), mode, page, scale);
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
        return location;
    }

    private String iterateMetric(final String context, final String metric, final int value) {
        String toMetric = metric;
        final List<String> metrics = getMetrics(context);
        if (!metrics.isEmpty()) {
            final int indexOf = metrics.indexOf(metric);
            if (indexOf >= 0) {
                toMetric = metrics.get((indexOf + value + metrics.size()) % metrics.size());
            } else {
                toMetric = metrics.get(0);
            }
        }
        return toMetric;
    }

    private List<String> getMetrics(final String context) {
        final Pattern patternMetricName = Pattern.compile("(.*)\\.(.*)\\.xml");  // filename pattern is known
        final TimeHistogram histogram = (TimeHistogram) AppNaming.lookup(App.Application.LOOKUP, context);
        final File folder = new File(histogram.getFolder());
        final Collection<String> metrics = new TreeSet<>();
        for (final File file : new FindInFolderQuery(folder, "*.xml", false).getFound()) {
            final Matcher matcher = patternMetricName.matcher(file.getName());
            if (matcher.matches()) {
                metrics.add(matcher.group(1));  // extract the metric name from the filename
            }
            metrics.remove(context);  // ignore the default filename for the context
        }
        return new ArrayList<>(metrics);
    }
}
