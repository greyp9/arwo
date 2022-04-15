package io.github.greyp9.arwo.core.env;

import java.util.List;

/**
 * Container for a collection of information about the process environment, including share information which may be
 * used to reconstitute a secret.
 */
public final class EnvironmentStore {
    private final List<EnvironmentShare> shares;

    public List<EnvironmentShare> getShares() {
        return shares;
    }

    public EnvironmentStore(final List<EnvironmentShare> shares) {
        this.shares = shares;
    }
}
