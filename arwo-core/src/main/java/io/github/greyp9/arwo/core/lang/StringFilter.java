package io.github.greyp9.arwo.core.lang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Pattern;

public class StringFilter {
    private final Collection<Pattern> includes;
    private final Collection<Pattern> excludes;

    public StringFilter(final Collection<String> includes, final Collection<String> excludes) {
        this.includes = new ArrayList<Pattern>();
        this.excludes = new ArrayList<Pattern>();
        for (final String include : includes) {
            this.includes.add(Pattern.compile(include));
        }
        for (final String exclude : excludes) {
            this.excludes.add(Pattern.compile(exclude));
        }
    }

    public final boolean matches(final String value) {
        boolean included = false;
        boolean excluded = false;
        for (final Pattern include : includes) {
            if (include.matcher(value).matches()) {
                included = true;
                break;
            }
        }
        for (final Pattern exclude : excludes) {
            if (exclude.matcher(value).matches()) {
                excluded = true;
                break;
            }
        }
        return (included && !excluded);
    }
}
