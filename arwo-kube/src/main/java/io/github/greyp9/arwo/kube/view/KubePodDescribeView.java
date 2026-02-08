package io.github.greyp9.arwo.kube.view;

import com.google.gson.JsonElement;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.core.JsonNav;
import io.github.greyp9.arwo.kube.core.JsonU;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Pod;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;

public final class KubePodDescribeView extends KubeView {
    private final String namespace;
    private final String podName;
    private final String path;

    public KubePodDescribeView(final ServletHttpRequest httpRequest, final AppUserState userState,
                               final KubeConnectionResource resource,
                               final String namespace, final String podName, final String path) {
        super(httpRequest, userState, resource);
        this.namespace = namespace;
        this.podName = podName;
        this.path = path;
    }

    @Override
    protected HttpResponse addContentTo(final Element html) throws IOException {
        final Element header = new XPather(html.getOwnerDocument(), null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html.getOwnerDocument(), null).getElement(Html.XPath.CONTENT);
        addMenuContext(header);  // navigation within json representation of describe data
        final ResourceCache cache = getUserState().getCache();
        final String cacheKey = String.join(Http.Token.SLASH, namespace, podName);
        final Object cachedValue = cache.getObject(cacheKey, this::getJsonElement);
        final JsonElement jsonElement = Value.asOptional(cachedValue, JsonElement.class).orElse(null);
        return (jsonElement == null) ? null
                : JsonNav.toHttpResponse(jsonElement, Value.split(Http.Token.SLASH, path), content);
    }

    private void addMenuContext(final Element html) throws IOException {
        final ResourceCache cache = getUserState().getCache();
        final String cacheKey = String.join(Http.Token.SLASH, namespace, podName);
        final String base = String.format("%s/%s/pods/%s/%s/describe/",
                getHttpRequest().getBaseURI(), getResource().getName(), namespace, podName);
        final Object cachedValue = cache.getObject(cacheKey, this::getJsonElement);
        final JsonElement jsonElement = Value.asOptional(cachedValue, JsonElement.class).orElse(null);
        if (jsonElement != null) {
            final KubeDocMenuFactory kubeDocMenuFactory = new KubeDocMenuFactory(base, jsonElement);
            final MenuItem menuItem = kubeDocMenuFactory.create()
                    .applyFrom(getUserState().getMenuSystemState());
            final MenuHtml menuHtml = new MenuHtml(getHttpRequest(),
                    getUserState().getBundle(), getUserState().getSubmitID(), AppHtmlView.STYLE_HOME);
            menuHtml.addTo(html, false, "v", Collections.singletonList(menuItem));
/*
            final MenuSystem menuSystemD = new MenuSystem(
                    getUserState().getSubmitID(), new KubeDocMenuFactory(base, jsonElement));
            menuSystemD.get(getHttpRequest().getServletPath(), "kube");  // init (since it is dynamic)
            menuSystemD.applyState(getUserState().getMenuSystemState());
            final MenuView menuView = new MenuView(null, getHttpRequest(), menuSystemD);  // render
            menuView.addContentTo(html, "kube", false);
*/
        }
    }

    private JsonElement getJsonElement() {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        try {
            final V1Pod v1Pod = api.readNamespacedPod(podName, namespace, null);
            return JsonU.toJsonTree(v1Pod);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            return null;
        }
    }
}
