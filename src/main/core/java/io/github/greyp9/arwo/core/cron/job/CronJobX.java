package io.github.greyp9.arwo.core.cron.job;

import org.w3c.dom.Element;

public class CronJobX {
    private final String name;
    private final boolean enabled;
    private final CronJob job;
    private final Element element;

    public final String getName() {
        return name;
    }

    public final boolean isEnabled() {
        return enabled;
    }

    public final CronJob getJob() {
        return job;
    }

    public final Element getElement() {
        return element;
    }

    public CronJobX(final String name, final boolean enabled, final CronJob job, final Element element) {
        this.name = name;
        this.enabled = enabled;
        this.job = job;
        this.element = element;
    }
}
