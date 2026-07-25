package io.github.greyp9.arwo.core.task.type.http;

import io.github.greyp9.arwo.core.task.core.Task;

import java.io.File;
import java.util.Date;

public class HttpTask extends Task {
    private final String resourceCert;
    private final String method;
    private final String url;
    private final String authorization;
    private final String transform;

    public HttpTask(final String name, final Date dateSubmit, final String resourceCert,
                    final String method, final String url, final String authorization,
                    final String transform) {
        super(name, dateSubmit);
        this.resourceCert = resourceCert;
        this.method = method;
        this.url = url;
        this.authorization = authorization;
        this.transform = transform;
    }

    public final String getResourceCert() {
        return resourceCert;
    }

    public final String getMethod() {
        return method;
    }

    public final String getUrl() {
        return url;
    }

    public final String getAuthorization() {
        return authorization;
    }

    public final String getTransform() {
        return transform;
    }

    @Override
    public final Runnable createRunnable(final File ignored) {
        return new HttpRunnable(this);
    }
}
