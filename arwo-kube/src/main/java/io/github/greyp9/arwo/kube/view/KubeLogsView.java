package io.github.greyp9.arwo.kube.view;

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
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.xed.widget.XedWidgetKubeLog;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Locale;
import java.util.Properties;

public class KubeLogsView extends KubeView {

    private final String namespace;
    private final String podName;
    private final String containerName;

    public KubeLogsView(final ServletHttpRequest httpRequest, final AppUserState userState,
                        final KubeConnectionResource resource,
                        final String namespace, final String podName, final String containerName) {
        super(httpRequest, userState, resource);
        this.namespace = namespace;
        this.podName = podName;
        this.containerName = containerName;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        try {
            final XedFactory xedFactory = getUserState().getXedFactory();
            final Locale locale = getUserState().getLocale();
            final XedWidgetKubeLog xedWidgetKubeLog = new XedWidgetKubeLog(xedFactory, locale);
            final Properties properties = getUserState().getKube().getProperties();
            xedWidgetKubeLog.applyFrom(properties);
            final byte[] payload = UTF8Codec.toBytes(api.readNamespacedPodLog(
                    podName, namespace, containerName, null, null, null,
                    Boolean.toString(xedWidgetKubeLog.getValueBoolean("/action:kubeLog/action:pretty")),
                    xedWidgetKubeLog.getValueBoolean("/action:kubeLog/action:previous"),
                    null,
                    xedWidgetKubeLog.getValueInt("/action:kubeLog/action:tailLines"),
                    xedWidgetKubeLog.getValueBoolean("/action:kubeLog/action:timestamps")));
            if (payload == null) {
                return HttpResponseU.to404();
            } else {
                final long lastModified = getHttpRequest().getDate().getTime() / DurationU.Const.ONE_SECOND_MILLIS;
                final FileMetaData metaData = new FileMetaData(null, payload.length, lastModified, false);
                return HttpResponseU.to200(new MetaFile(
                        metaData, Http.Mime.TEXT_PLAIN_UTF8, new ByteArrayInputStream(payload)));
            }
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }
}
