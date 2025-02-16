package io.github.greyp9.arwo.kube.core;

import java.time.OffsetDateTime;
import java.util.Date;

public final class KubeU {

    private KubeU() {
    }

    public static Date toDate(final OffsetDateTime offsetDateTime) {
        return (offsetDateTime == null) ? null : new Date(offsetDateTime.toInstant().toEpochMilli());
    }
}
