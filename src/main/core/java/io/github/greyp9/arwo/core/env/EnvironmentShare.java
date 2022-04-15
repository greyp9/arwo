package io.github.greyp9.arwo.core.env;

/**
 * Container for a single piece of information about the process environment, including share information which may be
 * used to reconstitute a secret.
 */
public final class EnvironmentShare {
    private final EnvironmentAtom atom;
    private final byte[] share;

    public EnvironmentAtom getAtom() {
        return atom;
    }

    public byte[] getShare() {
        return share;
    }

    public EnvironmentShare(final EnvironmentAtom atom, final byte[] share) {
        this.atom = atom;
        this.share = share;
    }
}

