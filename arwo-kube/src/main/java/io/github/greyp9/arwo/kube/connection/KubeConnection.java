package io.github.greyp9.arwo.kube.connection;

import io.kubernetes.client.openapi.apis.CoreV1Api;

import java.util.Date;

public final class KubeConnection {
    private final CoreV1Api coreV1Api;
    private final long dateOpen;
    private final long dateLast;

    public CoreV1Api getCoreV1Api() {
        return coreV1Api;
    }

    public Date getDateOpen() {
        return new Date(dateOpen);
    }

    public Date getDateLast() {
        return new Date(dateLast);
    }

    public KubeConnection(final CoreV1Api coreV1Api) {
        this.coreV1Api = coreV1Api;
        this.dateOpen = new Date().getTime();
        this.dateLast = dateOpen;
    }
}
