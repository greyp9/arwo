package io.github.greyp9.arwo.core.env.eval;

import io.github.greyp9.arwo.core.env.EnvironmentAtom;
import io.github.greyp9.arwo.core.expr.Node;

public final class AtomNode extends Node {
    private final EnvironmentAtom atom;

    public EnvironmentAtom getAtom() {
        return atom;
    }

    public AtomNode(final EnvironmentAtom atom) {
        this.atom = atom;
    }

    @Override
    public String render() {
        return String.format("atom(ordinal=%d key=%s value=%s)",
                atom.getOrdinal(), atom.getKey(), atom.getValue());
    }
}
