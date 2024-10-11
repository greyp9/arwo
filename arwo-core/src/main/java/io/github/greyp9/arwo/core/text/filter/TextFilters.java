package io.github.greyp9.arwo.core.text.filter;

import java.util.ArrayList;
import java.util.Collection;

public class TextFilters {
    private final Collection<String> includes;
    private final Collection<String> excludes;
    private final Collection<String> expressions;

    public final Collection<String> getIncludes() {
        return includes;
    }

    public final Collection<String> getExcludes() {
        return excludes;
    }

    public final Collection<String> getExpressions() {
        return expressions;
    }

    public TextFilters() {
        this.includes = new ArrayList<String>();
        this.excludes = new ArrayList<String>();
        this.expressions = new ArrayList<String>();
    }

    public final boolean isData() {
        final boolean isDataIncludes = (!includes.isEmpty());
        final boolean isDataExcludes = (!excludes.isEmpty());
        final boolean isDataExpressions = (!expressions.isEmpty());
        return (isDataIncludes || isDataExcludes || isDataExpressions);
    }
}
