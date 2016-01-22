package io.github.greyp9.arwo.core.alert.link;

import java.util.Arrays;
import java.util.Collection;

public class AlertLinks {
    private final Collection<AlertLink> links;

    public final Collection<AlertLink> getLinks() {
        return links;
    }

    public AlertLinks(final AlertLink... links) {
        this.links = Arrays.asList(links);
    }
}
