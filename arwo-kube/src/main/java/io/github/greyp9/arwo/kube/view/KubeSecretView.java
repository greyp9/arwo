package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.data.SecretDataRowSetSource;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Secret;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Map;

public class KubeSecretView extends KubeView {
    private final String endpoint;
    private final String namespace;
    private final String name;
    private final String datum;

    public KubeSecretView(final ServletHttpRequest httpRequest, final AppUserState userState,
                          final KubeConnectionResource resource, final String endpoint,
                          final String namespace, final String name, final String datum) {
        super(httpRequest, userState, resource);
        this.endpoint = endpoint;
        this.namespace = namespace;
        this.name = name;
        this.datum = datum;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final String key = String.format("%s/%s/%s", endpoint, namespace, name);
        final ResourceCache cache = getUserState().getCache();
        final Object object = cache.getObject(key, this::getSecret);
        final V1Secret v1Secret = Value.as(object, V1Secret.class);

        return (datum == null) ? addSecretTo(v1Secret, html) : addDatumTo(v1Secret);
    }

    protected final HttpResponse addSecretTo(final V1Secret v1Secret, final Element html) throws IOException {
        final SecretDataRowSetSource rowSetSource =
                new SecretDataRowSetSource(getHttpRequest().getBaseURI(), endpoint, v1Secret);
        try {
            final RowSet rowSet = rowSetSource.getRowSet();
            final UserStateTable table = new UserStateTable(getUserState(), null, getHttpRequest().getDate(), true);
            table.toTableView(rowSet).addContentTo(html);
        } catch (final ApiException e) {
            // final String id = "kube.ok-" + XsdDateU.toXSDZMillis(getHttpRequest().getDate());
            // final AlertActions alertActions = new AlertActions(id, "kube.ok");
            final AlertActions alertActions = null;  // persistent to aid diagnosis
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getResponseBody(), alertActions));
        } catch (final Exception e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }

    private HttpResponse addDatumTo(final V1Secret v1Secret) {
        final Map<String, byte[]> data = v1Secret.getData();
        final byte[] payload = (data == null) ? null : data.get(datum);
        if (payload == null) {
            return HttpResponseU.to404();
        } else {
            final FileMetaData metaData = new FileMetaData(null, payload.length, 0L, 0L, FileMetaData.Type.FILE);
            return HttpResponseU.to200(new MetaFile(
                    metaData, Http.Mime.TEXT_PLAIN_UTF8, new ByteArrayInputStream(payload)));
        }
    }

    private V1Secret getSecret() {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        try {
            return api.readNamespacedSecret(name, namespace, null);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            return null;
        }
    }
}
