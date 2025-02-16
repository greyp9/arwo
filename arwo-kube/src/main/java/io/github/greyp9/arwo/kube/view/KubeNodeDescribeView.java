package io.github.greyp9.arwo.kube.view;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.core.JsonU;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Node;
import org.w3c.dom.Element;

public class KubeNodeDescribeView extends KubeView {
    private final String nodeName;

    public KubeNodeDescribeView(final ServletHttpRequest httpRequest, final AppUserState userState,
                                final KubeConnectionResource resource, final String nodeName) {
        super(httpRequest, userState, resource);
        this.nodeName = nodeName;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        try {
            final V1Node v1Node = api.readNode(nodeName, null);
            final byte[] payload = UTF8Codec.toBytes(JsonU.toJsonPretty(v1Node));
            final long lastModified = getHttpRequest().getDate().getTime() / DurationU.Const.ONE_SECOND_MILLIS;
            final FileMetaData metaData = new FileMetaData(null, payload.length, lastModified, false);
            return HttpResponseU.to200(new MetaFile(
                    metaData, Http.Mime.APP_JSON, new ByteArrayInputStream(payload)));
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }
}
