package io.github.greyp9.arwo.core.table.compare;

import io.github.greyp9.arwo.core.lang.CompareU;

import java.io.Serializable;
import java.util.Date;

public class CellComparator implements Serializable, java.util.Comparator<Object> {
    private static final long serialVersionUID = 2065318622440179081L;

    @SuppressWarnings("PMD.CyclomaticComplexity")
    @Override
    public final int compare(final Object left, final Object right) {
        int compare;
        if (left == null) {
            compare = (right == null) ? 0 : -1;
        } else if (right == null) {
            compare = 1;
        } else if ((left instanceof String) && ((right instanceof String))) {
            compare = CompareU.compare((String) left, (String) right);
        } else if ((left instanceof Integer) && ((right instanceof Integer))) {
            compare = CompareU.compare((Integer) left, (Integer) right);
        } else if ((left instanceof Long) && ((right instanceof Long))) {
            compare = CompareU.compare((Long) left, (Long) right);
        } else if ((left instanceof Date) && ((right instanceof Date))) {
            compare = CompareU.compare((Date) left, (Date) right);
        } else {
            compare = left.toString().compareTo(right.toString());
        }
        return compare;
    }
}
