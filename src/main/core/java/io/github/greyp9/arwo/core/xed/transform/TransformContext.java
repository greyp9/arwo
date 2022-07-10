package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.xpath.XPather;

import java.security.Key;

public class TransformContext {
    private final Key key;
    private final XPather xpather;

    public final Key getKey() {
        return key;
    }

    public final XPather getXPather() {
        return xpather;
    }

    public TransformContext(final Key key, final XPather xpather) {
        this.key = key;
        this.xpather = xpather;
    }
}
