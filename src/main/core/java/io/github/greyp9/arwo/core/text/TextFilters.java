package io.github.greyp9.arwo.core.text;

import java.util.ArrayList;
import java.util.Collection;

public class TextFilters {
    private final Collection<String> includes;
    private final Collection<String> excludes;

    public final Collection<String> getIncludes() {
        return includes;
    }

    public final Collection<String> getExcludes() {
        return excludes;
    }

    public TextFilters() {
        this.includes = new ArrayList<String>();
        this.excludes = new ArrayList<String>();
    }
}
