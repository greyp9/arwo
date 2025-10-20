package io.github.greyp9.arwo.kube.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.connection.KubeConnectionFactory;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.view.KubeContainersView;
import io.github.greyp9.arwo.kube.view.KubeEndpointView;
import io.github.greyp9.arwo.kube.view.KubeFSView;
import io.github.greyp9.arwo.kube.view.KubeLogsView;
import io.github.greyp9.arwo.kube.view.KubeNodeDescribeView;
import io.github.greyp9.arwo.kube.view.KubeNodesView;
import io.github.greyp9.arwo.kube.view.KubePodDescribeView;
import io.github.greyp9.arwo.kube.view.KubePodsView;
import io.github.greyp9.arwo.kube.view.KubeSecretView;
import io.github.greyp9.arwo.kube.view.KubeSecretsView;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class KubeHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public KubeHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doGetInternal();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to500(e.getMessage());
        }
        return httpResponse;
    }

    private HttpResponse doGetInternal() throws IOException {
        final HttpResponse httpResponse;
        // we want the untranslated path info to enable JSON path traversal (handle "/" in key names)
        final String pathInfoRaw = httpRequest.getHttpRequest().getResource()
                .substring(httpRequest.getBaseURI().length());
        final Pather patherEndpoint = new Pather(pathInfoRaw);  // new Pather(httpRequest.getPathInfo());
        final String kubeEndpoint = patherEndpoint.getLeftToken();
        final String kubeResource = patherEndpoint.getRight();
        if (Value.isEmpty(kubeEndpoint)) {
            httpResponse = new KubeEndpointView(httpRequest, userState, null).doGetResponse();
        } else {
            httpResponse = doGet(kubeEndpoint, kubeResource);
        }
        return httpResponse;
    }

    private static final Pattern PATTERN_NODES = Pattern.compile("/nodes/");
    private static final Pattern PATTERN_NODE_PODS = Pattern.compile("/nodes/(.+)/pods/");
    private static final Pattern PATTERN_NODE_DESCRIBE = Pattern.compile("/nodes/(.+)/describe(/.+)*/");
    private static final Pattern PATTERN_PODS = Pattern.compile("/pods/");
    private static final Pattern PATTERN_POD_DESCRIBE = Pattern.compile("/pods/(.+)/(.+)/describe(/.+)*/");
    private static final Pattern PATTERN_CONTAINERS = Pattern.compile("/pods/(.+)/(.+)/containers/");
    private static final Pattern PATTERN_CONTAINER_LOGS = Pattern.compile("/pods/(.+)/(.+)/containers/(.+)/logs/");
    private static final Pattern PATTERN_CONTAINER_KFS = Pattern.compile("/pods/(.+)/(.+)/containers/(.+)/kfs/(.*)");
    private static final Pattern PATTERN_SECRET_DATA = Pattern.compile("/secrets/(.+)/(.+)/(.+)");
    private static final Pattern PATTERN_SECRET = Pattern.compile("/secrets/(.+)/(.+)/");
    private static final Pattern PATTERN_SECRETS_NS = Pattern.compile("/secrets/(.+)/");
    private static final Pattern PATTERN_SECRETS = Pattern.compile("/secrets/");

    private HttpResponse doGet(final String kubeEndpoint, final String kubeResource) throws IOException {
        final KubeConnectionFactory factory = new KubeConnectionFactory(httpRequest, userState);
        final ConnectionCache cache = userState.getKube().getCache();
        final KubeConnectionResource resource = (KubeConnectionResource) cache.getResource(kubeEndpoint, factory);

        final Matcher matcherNodePods = PATTERN_NODE_PODS.matcher(kubeResource);
        final Matcher matcherNodeDescribe = PATTERN_NODE_DESCRIBE.matcher(kubeResource);
        final Matcher matcherNodes = PATTERN_NODES.matcher(kubeResource);
        final Matcher matcherPodDescribe = PATTERN_POD_DESCRIBE.matcher(kubeResource);
        final Matcher matcherPods = PATTERN_PODS.matcher(kubeResource);
        final Matcher matcherContainers = PATTERN_CONTAINERS.matcher(kubeResource);
        final Matcher matcherContainerLogs = PATTERN_CONTAINER_LOGS.matcher(kubeResource);
        final Matcher matcherContainerKFS = PATTERN_CONTAINER_KFS.matcher(kubeResource);
        final Matcher matcherSecretData = PATTERN_SECRET_DATA.matcher(kubeResource);
        final Matcher matcherSecret = PATTERN_SECRET.matcher(kubeResource);
        final Matcher matcherSecretsNS = PATTERN_SECRETS_NS.matcher(kubeResource);
        final Matcher matcherSecrets = PATTERN_SECRETS.matcher(kubeResource);

        final HttpResponse httpResponse;
        if (resource == null) {
            httpResponse = HttpResponseU.to404();
        } else if (matcherContainerLogs.matches()) {
            final String namespace = matcherContainerLogs.group(1);
            final String podName = matcherContainerLogs.group(2);
            final String containerName = matcherContainerLogs.group(3);
            httpResponse = new KubeLogsView(
                    httpRequest, userState, resource, namespace, podName, containerName).doGetResponse();
        } else if (matcherContainerKFS.matches()) {
            final String namespace = matcherContainerKFS.group(1);
            final String podName = matcherContainerKFS.group(2);
            final String containerName = matcherContainerKFS.group(3);
            final String path = Http.Token.SLASH + matcherContainerKFS.group(4);
            httpResponse = new KubeFSView(
                    httpRequest, userState, resource, namespace, podName, containerName, path).doGetResponse();
        } else if (matcherPodDescribe.matches()) {
            final String namespace = matcherPodDescribe.group(1);
            final String podName = matcherPodDescribe.group(2);
            final String path = matcherPodDescribe.group(3);
            httpResponse = new KubePodDescribeView(
                    httpRequest, userState, resource, namespace, podName, path).doGetResponse();
        } else if (matcherContainers.matches()) {
            final String namespace = matcherContainers.group(1);
            final String podName = matcherContainers.group(2);
            httpResponse = new KubeContainersView(
                    httpRequest, userState, resource, namespace, podName).doGetResponse();
        } else if (matcherPods.matches()) {
            httpResponse = new KubePodsView(httpRequest, userState, resource, null).doGetResponse();
        } else if (matcherSecretData.matches()) {
            final String namespace = matcherSecretData.group(1);
            final String name = matcherSecretData.group(2);
            final String datum = matcherSecretData.group(3);
            httpResponse = new KubeSecretView(
                    httpRequest, userState, resource, kubeEndpoint, namespace, name, datum).doGetResponse();
        } else if (matcherSecret.matches()) {
            final String namespace = matcherSecret.group(1);
            final String name = matcherSecret.group(2);
            httpResponse = new KubeSecretView(
                    httpRequest, userState, resource, kubeEndpoint, namespace, name, null).doGetResponse();
        } else if (matcherSecretsNS.matches()) {
            final String namespace = matcherSecretsNS.group(1);
            httpResponse = new KubeSecretsView(httpRequest, userState, resource, namespace).doGetResponse();
        } else if (matcherSecrets.matches()) {
            httpResponse = new KubeSecretsView(httpRequest, userState, resource, null).doGetResponse();
        } else if (matcherNodeDescribe.matches()) {
            final String nodeName = matcherNodeDescribe.group(1);
            final String path = matcherNodeDescribe.group(2);
            httpResponse = new KubeNodeDescribeView(
                    httpRequest, userState, resource, nodeName, path).doGetResponse();
        } else if (matcherNodePods.matches()) {
            final String nodeName = matcherNodePods.group(1);
            httpResponse = new KubePodsView(httpRequest, userState, resource, nodeName).doGetResponse();
        } else if (matcherNodes.matches()) {
            httpResponse = new KubeNodesView(httpRequest, userState, resource).doGetResponse();
        } else {
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }


}
