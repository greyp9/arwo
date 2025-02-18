package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.core.JsonNav;
import io.github.greyp9.arwo.kube.core.JsonU;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Node;
import org.w3c.dom.Element;

import java.io.IOException;

public class KubeNodeDescribeView extends KubeView {
    private final String nodeName;
    private final String path;

    public KubeNodeDescribeView(final ServletHttpRequest httpRequest, final AppUserState userState,
                                final KubeConnectionResource resource, final String nodeName, final String path) {
        super(httpRequest, userState, resource);
        this.nodeName = nodeName;
        this.path = path;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        try {
            final V1Node v1Node = api.readNode(nodeName, null);
            return JsonNav.toHttpResponse(JsonU.toJsonTree(v1Node), Value.split(Http.Token.SLASH, path), html);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }
}
