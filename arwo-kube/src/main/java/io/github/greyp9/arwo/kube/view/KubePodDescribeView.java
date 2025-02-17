package io.github.greyp9.arwo.kube.view;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.MathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.core.JsonU;
import io.github.greyp9.arwo.kube.core.KubeException;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Pod;
import org.w3c.dom.Element;

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
            final JsonElement jsonTree = JsonU.toJsonTree(v1Pod);
            final byte[] payload = UTF8Codec.toBytes(toPayloadPretty(jsonTree, new Pather(path), html));
            if (payload == null) {
                return null;
            }
            final long lastModified = getHttpRequest().getDate().getTime() / DurationU.Const.ONE_SECOND_MILLIS;
            final FileMetaData metaData = new FileMetaData(null, payload.length, lastModified, false);
            return HttpResponseU.to200(new MetaFile(
                    metaData, Http.Mime.APP_JSON, new ByteArrayInputStream(payload)));
        } catch (final KubeException e) {
            return HttpResponseU.to404();
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }

    private String toPayloadPretty(final JsonElement jsonTree, final Pather pather,
                                   final Element html) throws KubeException {
        final String leftToken = pather.getLeftToken();
        final Pather patherSub = new Pather(pather.getRight());
        if (Value.isEmpty(leftToken) && jsonTree.isJsonObject()) {
            final JsonObject jsonObject = jsonTree.getAsJsonObject();
            final Set<Map.Entry<String, JsonElement>> entries = jsonObject.entrySet();
            for (Map.Entry<String, JsonElement> entry : entries) {
                final Element div = ElementU.addElement(html, Html.DIV, null);
                ElementU.addElement(div, Html.A, entry.getKey(), NTV.create(Html.HREF, entry.getKey() + "/"));
            }
            final Element div = ElementU.addElement(html, Html.DIV, null);
            ElementU.addElement(div, Html.A, "view json", NTV.create(Html.HREF, "-/"));
            return null;
        } else if (Value.isEmpty(leftToken) && jsonTree.isJsonArray()) {
            final JsonArray jsonArray = jsonTree.getAsJsonArray();
            for (int i = 0; (i < jsonArray.size()); ++i) {
                final Element div = ElementU.addElement(html, Html.DIV, null);
                ElementU.addElement(div, Html.A, Integer.toString(i), NTV.create(Html.HREF, i + "/"));
            }
            final Element div = ElementU.addElement(html, Html.DIV, null);
            ElementU.addElement(div, Html.A, "view json", NTV.create(Html.HREF, "-/"));
            return null;
        } else if ("-".equals(leftToken)) {
            return JsonU.toPretty(jsonTree);
        } else if (jsonTree.isJsonObject()) {
            final JsonElement jsonElement = jsonTree.getAsJsonObject().get(leftToken);
            return toPayloadPretty(jsonElement, patherSub, html);
        } else if (jsonTree.isJsonArray()) {
            final JsonArray jsonArray = jsonTree.getAsJsonArray();
            final int index = Value.parseInt(leftToken, -1);
            if (MathU.isInBound(0, index, jsonArray.size() - 1)) {
                final JsonElement jsonElement = jsonTree.getAsJsonArray().get(index);
                return toPayloadPretty(jsonElement, patherSub, html);
            } else {
                throw new KubeException(leftToken);
            }
        } else if (jsonTree.isJsonPrimitive()) {
            return jsonTree.getAsJsonPrimitive().toString();
        } else {
            throw new IllegalStateException(leftToken);
        }
    }
}
