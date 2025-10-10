package io.github.greyp9.arwo.kube.connection;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesX;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.apis.CoreV1Api;

import java.util.Date;
import java.util.Properties;

public final class KubeConnection {
    private final ApiClient apiClient;
    private final CoreV1Api coreV1Api;
    private final long dateOpen;
    private final Properties properties;
    private final PropertiesX propertiesX;

    public ApiClient getApiClient() {
        return apiClient;
    }

    public CoreV1Api getCoreV1Api() {
        return coreV1Api;
    }

    public Date getDateOpen() {
        return new Date(dateOpen);
    }

    public Properties getProperties() {
        return properties;
    }

    public KubeConnection(final ApiClient apiClient, final CoreV1Api coreV1Api) {
        this.apiClient = apiClient;
        this.coreV1Api = coreV1Api;
        this.dateOpen = new Date().getTime();
        this.properties = new Properties();
        this.propertiesX = new PropertiesX(properties);
        propertiesX.setLong(App.Connection.DATE_LAST, dateOpen);
        propertiesX.setLong(App.Connection.COUNT, 0L);
        propertiesX.setLong(App.Connection.MILLIS, 0L);
    }

    public Date getDateLast() {
        return new Date(propertiesX.getLong(App.Connection.DATE_LAST));
    }

    public long getCount() {
        return propertiesX.getLong(App.Connection.COUNT);
    }

    public long getMillis() {
        return propertiesX.getLong(App.Connection.MILLIS);
    }

    public void update(final Date date) {
        // propertiesX.setLong(App.Connection.DATE_LAST, date.getTime());  // enable connection refresh via timeout
        propertiesX.addLong(App.Connection.COUNT, 1L);
        final long millis = SystemU.currentTimeMillis() - date.getTime();
        propertiesX.addLong(App.Connection.MILLIS, millis);
    }
}
