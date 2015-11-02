package io.github.greyp9.arwo.core.table.filter;

import io.github.greyp9.arwo.core.table.compare.CellComparator;

public class Filter {
    private final int index;
    private final String name;
    private final Operator operator;
    private final Object value;

    public Filter(final int index, final String name, final Operator operator, final Object value) {
        this.index = index;
        this.name = name;
        this.operator = operator;
        this.value = value;
    }

    public final int getIndex() {
        return index;
    }

    public final String getName() {
        return name;
    }

    public final Operator getOperator() {
        return operator;
    }

    public final Object getValue() {
        return value;
    }

    public final boolean matches(final CellComparator cellComparator, final Object valueToMatch) {
        final int compare = cellComparator.compare(valueToMatch, value);
        boolean matches = false;
        switch (operator) {
            case EQ:
                matches = (compare == 0);
                break;
            case LT:
                matches = (compare < 0);
                break;
            case GT:
                matches = (compare > 0);
                break;
            case NEQ:
                matches = (compare != 0);
                break;
            case LEQ:
                matches = (compare <= 0);
                break;
            case GEQ:
                matches = (compare >= 0);
                break;
            default:
                break;
        }
        return matches;
    }

/*
    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Operator toOperator(final String name) {
        try {
            return Filter.Operator.valueOf(name);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }
*/

    public enum Operator {
        EQ, LT, GT, NEQ, LEQ, GEQ
    }
}
