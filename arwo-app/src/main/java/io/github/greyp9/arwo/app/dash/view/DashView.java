package io.github.greyp9.arwo.app.dash.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.connect.AppConnectionView;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.props.AppPropertiesView;
import io.github.greyp9.arwo.app.cron.view.CronActiveView;
import io.github.greyp9.arwo.app.xed.view.XedUnsavedView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu2.core.MenuSession;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.vm.process.RuntimeU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;

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
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);
        // context-specific content
        final AppTitle title = AppTitle.Factory.getHostLabel(httpRequest, request.getBundle());
        addMenus(header);
        addContentTo(content);
        return new AppHtmlView(httpRequest, userState, title)
                .title(header)
                .actionRefresh(header)
                .actionTextExpression(header)
                .alerts(header)
                .statusBar(footer)
                .appHtml(html)
                .toHttpResponse(html);
    }

    private void addMenus(final Element header) {
        final MenuItem menu = new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU2, MENU_KEY, null,
                new MenuSession().toMenuItem(PathU.toPath(MENU_KEY, App.Target.SESSION)))
                .applyFrom(userState.getMenuSystemState());
        new MenuHtml(httpRequest, userState.getBundle(), userState.getSubmitID(), STYLE_HOME)
                .addTo(header, true, "m", Collections.singletonList(menu));
    }

    private static final String MENU_KEY = "/menu2/dash";
    private static final String STYLE_HOME = "background-color: brown; color: white;";

    private void addContentTo(final Element html) throws IOException {
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
