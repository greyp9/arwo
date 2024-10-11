package io.github.greyp9.arwo.core.env;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Container for a collection of information about the process environment.
 *
 * Includes a persistence location, the source of the specified information, and a threshold of atoms that must match
 * in order to perform data recovery.
 */
public final class EnvironmentState {
    private final String resource;
    private final String expression;
    private final List<EnvironmentAtom> atoms;
    private final int threshold;

    public String getResource() {
        return resource;
    }

    public String getExpression() {
        return expression;
    }

    public List<EnvironmentAtom> getAtoms() {
        return atoms;
    }

    public int getThreshold() {
        return threshold;
    }

    public EnvironmentState(final String resource, final String expression,
                            final int threshold, final EnvironmentAtom... atoms) {
        this.resource = resource;
        this.expression = expression;
        this.atoms = new ArrayList<>(Arrays.asList(atoms));
        this.threshold = threshold;
    }

    public EnvironmentState(final String resource, final String expression,
                            final int threshold, final List<EnvironmentAtom> atoms) {
        this.resource = resource;
        this.expression = expression;
        this.atoms = atoms;
        this.threshold = threshold;
    }
}
