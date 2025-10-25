package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.text.TextU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.xed.widget.XedWidgetKubeLog;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;
import java.util.Locale;
import java.util.Optional;
import java.util.Properties;

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.ExcessiveImports" })
public abstract class KubeView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final KubeConnectionResource resource;
//    private final AppTitle title;
    private final MenuContext menuContext;

    public final ServletHttpRequest getHttpRequest() {
        return httpRequest;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final KubeConnectionResource getResource() {
        return resource;
    }

    public KubeView(final ServletHttpRequest httpRequest,
                    final AppUserState userState,
                    final KubeConnectionResource resource) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.resource = resource;
        final MenuSystem menuSystem = new MenuSystem(userState.getSubmitID(), new KubeAppMenuFactory());
        final MenuItem menuItem = menuSystem.get(httpRequest.getServletPath(), KubeAppMenuFactory.KUBE); // init
        menuSystem.applyState(userState.getMenuSystemState());  // apply state
        this.menuContext = new MenuContext(menuSystem, Collections.singletonList(menuItem));
    }

    public final HttpResponse doGetResponse() throws IOException {
        final XedFactory xedFactory = userState.getXedFactory();
        final Locale locale = userState.getLocale();
        final Properties properties = userState.getKube().getProperties();
        final XedWidgetKubeLog widget = new XedWidgetKubeLog(xedFactory, locale);
        widget.applyFrom(properties);

        final String labelContext = TextU.wrapBracket(
                Optional.ofNullable(resource).map(KubeConnectionResource::getName).orElse(""));
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(), labelContext,
                new NameTypeValue(App.Action.PRETTY, widget.getValue("/action:kubeLog/action:pretty")).toStringNV(),
                new NameTypeValue("previous", widget.getValue("/action:kubeLog/action:previous")).toStringNV(),
                new NameTypeValue("tailLines", widget.getValue("/action:kubeLog/action:tailLines")).toStringNV(),
                new NameTypeValue("timestamps", widget.getValue("/action:kubeLog/action:timestamps")).toStringNV());

        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        if (PropertiesU.isBoolean(userState.getProperties(), KubeAppMenuFactory.KUBE_LOG_OPTIONS)) {
            widget.addPropertyStripTo(body, userState.getSubmitID());
        }
        final HttpResponse httpResponse = addContentTo(body);
        return Optional.ofNullable(httpResponse)
                .orElse(new AppHtmlView(httpRequest, userState, appTitle, menuContext, App.Token.EMPTY).fixup(html));
    }

    /**
     * Insert content into HTML page appropriate to the context of the subclass.
     *
     * @param html the wrapper HTML document
     * @return the http response containing the formatted content to be served to the requester
     * @throws IOException on failures accessing requested resources
     */
    protected abstract HttpResponse addContentTo(Element html) throws IOException;

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
    static final String CONTEXT_KFS = "kfs";
    static final String CONTEXT_NODES = "nodes";
    static final String CONTEXT_PODS = "pods";

}
