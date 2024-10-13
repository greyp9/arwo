package io.github.greyp9.arwo.kube.connection;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.CursorKube;
import io.github.greyp9.arwo.core.connect.ConnectionFactory;
import io.github.greyp9.arwo.core.connect.ConnectionResource;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.util.ClientBuilder;
import io.kubernetes.client.util.KubeConfig;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.logging.Logger;

public class KubeConnectionFactory implements ConnectionFactory {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public KubeConnectionFactory(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.bundle = userState.getBundle();
        this.alerts = userState.getAlerts();
    }

    @Override
    public final ConnectionResource create(final String name) throws IOException {
        Logger.getLogger(getClass().getName()).finest(httpRequest.getDate().toString());
        ConnectionResource resource = null;
        final XedSession session = userState.getDocumentState().getSession(App.Servlet.SETTINGS);
        final CursorKube cursorKube = new CursorKube(session.getXed(), name);
        if (Value.isEmpty(name)) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.getString("KubeConnectionFactory.no.server")));
        } else if (cursorKube.getCursor() == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("KubeConnectionFactory.no.config", name)));
        } else {
            resource = getConnection(name, cursorKube.getCursor());
        }
        return resource;
    }

    @SuppressWarnings("PMD.CloseResource")
    private KubeConnectionResource getConnection(final String name, final XedCursor cursor) throws IOException {
        final String config = cursor.getValue(cursor.getChildInstance("config"));
        final String context = cursor.getValue(cursor.getChildInstance("context"));
        final String namespace = cursor.getValue(cursor.getChildInstance("namespace"));
        final File kubeConfigFile = new File(config);
        final KubeConfig kubeConfig = KubeConfig.loadKubeConfig(new FileReader(kubeConfigFile));
        if (!Value.isEmpty(context)) {
            kubeConfig.setContext(context);
        }
        final ApiClient apiClient = ClientBuilder.kubeconfig(kubeConfig).build();
        final CoreV1Api api = new CoreV1Api(apiClient);
        return new KubeConnectionResource(name, namespace, new KubeConnection(api));
    }
}
