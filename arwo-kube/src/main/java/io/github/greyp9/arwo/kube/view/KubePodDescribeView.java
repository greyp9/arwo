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
import io.kubernetes.client.openapi.models.V1Pod;
import org.w3c.dom.Element;

import java.io.IOException;

public class KubePodDescribeView extends KubeView {
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
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        try {
            final V1Pod v1Pod = api.readNamespacedPod(podName, namespace, null);
            return JsonNav.toHttpResponse(JsonU.toJsonTree(v1Pod), Value.split(Http.Token.SLASH, path), html);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }
}
