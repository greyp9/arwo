package io.github.greyp9.arwo.app.cron.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Locale;
import java.util.Properties;


public class CronView {
    private final AppRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public CronView(final ServletHttpRequest httpRequest, final AppUserState userState) {
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
        //new XedActionTextFilter(locale).addContentTo(html, submitID, properties);
    }

    private HttpResponse addContentTo(final Element html) throws IOException {
        new CronTabView(userState.getCronService(), request, userState).addContent(html);
        new CronActiveView(userState.getCronService(), request, userState).addContent(html, true);
        new CronInvocationView(request, userState).addContent(html);
        return null;
    }
}
