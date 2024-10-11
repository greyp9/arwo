package io.github.greyp9.arwo.core.version;

import java.io.Serializable;

public class VersionComparator implements Serializable, java.util.Comparator<String> {
    private static final long serialVersionUID = -1264761248256799001L;

    @Override
    public final int compare(final String left, final String right) {
        return VersionU.compare(left, right);
    }
}
