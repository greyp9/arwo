package io.github.greyp9.arwo.kafka.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedAction;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.kafka.client.ConsumeKafkaClient;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;
import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class KafkaServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 263712868668644295L;

    private final Logger logger = Logger.getLogger(getClass().getName());

    private transient AppState appState;
    private transient ConsumeKafkaClient consumeKafkaClient;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Context context = AppNaming.lookupSubcontext(getServletContext().getContextPath());
        synchronized (this) {
            this.appState = (AppState) AppNaming.lookup(context, App.Naming.APP_STATE);
            final ExecutorService executor = (ExecutorService) AppNaming.lookup(context, App.Naming.EXECUTOR_SERVICE);
            this.consumeKafkaClient = new ConsumeKafkaClient(executor, ServletU.fromServletConfig(config, "kafka."));
        }
        logger.info("init()");
    }

    @Override
    public final void destroy() {
        logger.info("destroy()");
        synchronized (this) {
            this.consumeKafkaClient.stop();
            this.consumeKafkaClient = null;
            this.appState = null;
        }
    }

    @Override
    protected final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        logger.info("doGet()");
        final ServletHttpRequest httpRequest = ServletU.read(request);

        // process request
        AppUserState userState;
        synchronized (this) {
            userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        }
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        new AlertsView(true, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                userState.getSubmitID()).addContentTo(body);
        ElementU.addElement(body, "h1", "Kafka");

        if (consumeKafkaClient.getReference().get() == null) {
            ElementU.addElement(body, Html.DIV, "kafkaRunnable RUNNING", NTV.create());
        } else {
            ElementU.addElement(body, Html.DIV, "kafkaRunnable NOT RUNNING", NTV.create());
        }

        final XedAction action = new XedAction(App.Actions.QNAME_EMPTY, userState.getXedFactory(), null);
        final Xed xedUI = action.getXedUI(userState.getLocus().getLocale());
        final XedPropertyPageView pageView = new XedPropertyPageView(null, new XedNav(xedUI).getRoot());
        final ActionFactory factory = new ActionFactory(
                userState.getSubmitID(), xedUI.getBundle(), App.Target.SESSION, "empty", null);
        final Collection<String> actions = Arrays.asList("start", "stop");
        final ActionButtons buttons = factory.create("empty", false, actions);
        new PropertyStripHtmlView(pageView, buttons).addContentDiv(body);

        final AppTitle title = AppTitle.Factory.getResourceLabel(
                httpRequest, userState.getBundle(), "Kafka");
        final Preferences preferences = new Preferences(userState.getConfig());
        final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
        final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
        new AppHtml(httpRequest).fixup(html, title, iconColor, theme);

        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        HttpResponse httpResponse = new HttpResponse(
                HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws IOException {
        logger.info("doPost()");
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState userState;
        synchronized (this) {
            userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        }

        consumeKafkaClient.update(httpRequest, userState.getSubmitID());
        logger.finest(appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate()).toString());
        HttpResponse httpResponse = HttpResponseU.to302(httpRequest.getURI());
        ServletU.write(httpResponse, response);
    }
}
