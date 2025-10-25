package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.cache.CacheRowSetSource;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.kube.cache.V1SecretCacher;
import io.github.greyp9.arwo.kube.connection.KubeConnection;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.data.SecretRowSetSource;
import io.kubernetes.client.openapi.ApiException;
import org.w3c.dom.Element;

import java.io.IOException;

@SuppressWarnings("PMD.ExcessiveImports")
public class KubeSecretsView extends KubeView {
    private final String namespace;

    @SuppressWarnings("WeakerAccess")
    public KubeSecretsView(final ServletHttpRequest httpRequest,
                           final AppUserState userState,
                           final KubeConnectionResource resource,
                           final String namespace) {
        super(httpRequest, userState, resource);
        this.namespace = namespace;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final KubeConnection connection = resource.getConnection();
        final String baseURI = getHttpRequest().getBaseURI();
        final String endpoint = getResource().getName();
        final String rowSetId = getHttpRequest().getURI();

        final V1SecretCacher cacher = new V1SecretCacher(getUserState().getCache(), endpoint);
        // getUserState().getCache().clear();  // if table isn't cached, then client will refetch it (keep-alive)
        final RowSetSource rowSetSource = new CacheRowSetSource(getUserState().getCache(),
                new SecretRowSetSource(cacher, connection, namespace, baseURI, endpoint), rowSetId);
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
}
