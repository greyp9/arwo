package io.github.greyp9.arwo.app.dash.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.connect.AppConnectionView;
import io.github.greyp9.arwo.app.core.view.props.AppPropertiesView;
import io.github.greyp9.arwo.app.cron.view.CronActiveView;
import io.github.greyp9.arwo.app.xed.view.XedUnsavedView;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.vm.process.RuntimeU;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Locale;
import java.util.Properties;

@SuppressWarnings("PMD.ExcessiveImports")
public class DashView {
    private final AppRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public DashView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = userState.getAppRequest(httpRequest);
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGetResponse() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // context-specific content
        final AppTitle title = AppTitle.Factory.getHostLabel(httpRequest, request.getBundle());
        addHeaderView(body, title);
        HttpResponse httpResponse = addContentTo(body);
        if (httpResponse == null) {
            // touch ups
            final boolean displayAlerts = (httpRequest.getHttpRequest().getHeader(App.Header.RESULT) == null);
            new AlertsView(displayAlerts, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                    userState.getSubmitID()).addContentTo(body);
            new StatusBarView(httpRequest, userState.getLocus()).addContentTo(body);
            new AppHtml(httpRequest).fixup(html, title);
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
        final MenuView menuView = new MenuView(request.getBundle(), httpRequest, userState.getMenuSystem());
        menuView.addContentTo(html, AppMenuFactory.Const.DASHBOARD, true);
        // context title (+ text filters)
        final Element divMenus = menuView.addTitle(html, title);
        divMenus.getClass();
        // settings property strips
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
        new XedActionTextFilter(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
    }

    private HttpResponse addContentTo(final Element html) throws IOException {
        addPropertiesView(html);
        new XedUnsavedView(httpRequest, userState).addContent(html, false);
        new ClasspathView(httpRequest, userState, getClass()).addContent(html);
        new CronActiveView(userState.getCronService(), request, userState).addContent(html, false);
        new AppConnectionView(httpRequest, userState, userState.getKube().getCache()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getS3().getCache()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getSSH().getCache()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getCIFS().getCache()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getInterop().getCache()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getWebDAV().getCache()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getJDBC().getCache()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getMail().getCacheSMTP()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getMail().getCacheIMAP()).addContentTo(html, false);
        new AppConnectionView(httpRequest, userState, userState.getMail().getCachePOP3()).addContentTo(html, false);
        return null;
    }

    private void addPropertiesView(final Element html) throws IOException {
        final DateX dateX = userState.getLocus().getDateX();
        final String startA = dateX.toString(userState.getDateAppStart());
        final String durationA = DurationU.duration(userState.getDateAppStart(), httpRequest.getDate());
        final String startU = dateX.toString(userState.getDateSessionStart());
        final String durationU = DurationU.duration(userState.getDateSessionStart(), httpRequest.getDate());
        final Bundle bundle = request.getBundle();
        final NameTypeValues properties = new NameTypeValues();
        properties.add(bundle.getString("DashView.runtime.name"), RuntimeU.getName());
        properties.add(bundle.getString("DashView.webapp.start"), startA);
        properties.add(bundle.getString("DashView.webapp.uptime"), durationA);
        properties.add(bundle.getString("DashView.session.start"), startU);
        properties.add(bundle.getString("DashView.session.uptime"), durationU);
        final AppPropertiesView view = new AppPropertiesView("dashPropertiesType", userState);  // i18n metadata
        view.addContentTo(html, null, bundle, properties);
    }
}
