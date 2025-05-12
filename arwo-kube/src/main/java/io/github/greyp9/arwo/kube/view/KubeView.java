package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.refresh.AppRefreshView;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.action.XedActionRefresh;
import io.github.greyp9.arwo.core.xed.action.XedActionTextExpression;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Locale;
import java.util.Properties;

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.ExcessiveImports" })
public abstract class KubeView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final KubeConnectionResource resource;
    private final Bundle bundle;

    public final ServletHttpRequest getHttpRequest() {
        return httpRequest;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final KubeConnectionResource getResource() {
        return resource;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public KubeView(final ServletHttpRequest httpRequest,
                    final AppUserState userState,
                    final KubeConnectionResource resource) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.resource = resource;
        this.bundle = userState.getBundle();
    }

    public final HttpResponse doGetResponse() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        new AppRefreshView(userState.getProperties()).addContentTo(html.getDocumentElement());
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        final AppTitle title = AppTitle.Factory.getHostLabel(httpRequest, bundle);
        addHeaderView(body, title);
        // context-specific content
        HttpResponse httpResponse = addContentTo(body);
        if (httpResponse == null) {
            // touch ups
            final boolean displayAlerts = (httpRequest.getHttpRequest().getHeader(App.Header.RESULT) == null);
            new AlertsView(displayAlerts, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                    userState.getSubmitID()).addContentTo(body);
            new StatusBarView(httpRequest, userState.getLocus()).addContentTo(body);
            final Preferences preferences = new Preferences(getUserState().getConfig());
            final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
            final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
            new AppHtml(httpRequest).fixup(html, title, iconColor, theme);
            // package into response
            final byte[] entity = DocumentU.toXHtml(html);
            final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
            final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
            final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
            httpResponse = new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
        }
        return httpResponse;
    }

    private void addHeaderView(final Element html, final AppTitle title) throws IOException {
        // context menu
        final MenuSystem menuSystem = new MenuSystem(userState.getSubmitID(), new KubeAppMenuFactory());
        menuSystem.get(httpRequest.getServletPath(), KubeAppMenuFactory.KUBE);  // init (since it is dynamic)
        menuSystem.applyState(userState.getMenuSystemState());  // apply state
        final MenuView menuView = new MenuView(bundle, httpRequest, menuSystem);  // render
        menuView.addContentTo(html, KubeAppMenuFactory.KUBE, true);
        addMenuContext(html);
        menuView.addTitle(html, title);
        // settings property strips
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
        new XedActionRefresh(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
        final TextFilters textFilters = userState.getTextFilters("");
        new XedActionTextExpression(userState.getXedFactory(), locale, textFilters).addContentTo(
                html, userState.getFiltersRecent(), submitID, properties);
        new XedActionTextFilter(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
    }

    /**
     * Insert content into HTML page appropriate to the context of the subclass.
     *
     * @param html the wrapper HTML document
     * @return the http response containing the formatted content to be served to the requester
     * @throws IOException on failures accessing requested resources
     */
    protected abstract HttpResponse addContentTo(Element html) throws IOException;

    protected void addMenuContext(final Element html) throws IOException {
    }

    // kube context table views
    static final String FIELD_SELECT = "select";
    static final String FIELD_CREATED = "created";
    static final String FIELD_HOST_IP = "hostIP";
    static final String FIELD_IMAGE = "image";
    static final String FIELD_INIT = "init";
    static final String FIELD_NAME = "name";
    static final String FIELD_NAMESPACE = "namespace";
    static final String FIELD_POD_IP = "podIP";
    static final String FIELD_PORTS = "ports";
    static final String FIELD_READY = "ready";
    static final String FIELD_RESTARTS = "restarts";
    static final String FIELD_STATE = "state";
    static final String FIELD_STATUS = "status";

    static final String CONTEXT_CONTAINERS = "containers";
    static final String CONTEXT_DESCRIBE = "describe";
    static final String CONTEXT_LOGS = "logs";
    static final String CONTEXT_NODES = "nodes";
    static final String CONTEXT_PODS = "pods";

}
