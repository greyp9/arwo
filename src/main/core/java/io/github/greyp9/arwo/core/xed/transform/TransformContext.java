package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.lang.CharU;
import io.github.greyp9.arwo.core.xpath.XPather;

public class TransformContext {
    private final char[] secret;
    private final XPather xpather;

    public final char[] getSecret() {
        return CharU.copy(secret);
    }

    public final XPather getXPather() {
        return xpather;
    }

    public TransformContext(final char[] secret, final XPather xpather) {
        this.secret = CharU.copy(secret);
        this.xpather = xpather;
    }
}
