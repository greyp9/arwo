package io.github.greyp9.arwo.core.file.find;

import java.io.File;
import java.io.FileFilter;
import java.util.regex.Pattern;

// i18nf
public class WildcardFileFilter implements FileFilter {
    private final Pattern pattern;

    public WildcardFileFilter(final Pattern pattern) {
        this.pattern = pattern;
    }

    public WildcardFileFilter(final String pattern) {
        this.pattern = toPattern(pattern);
    }

    @SuppressWarnings("PMD.LawOfDemeter")
    public final boolean accept(final File file) {
        return ((pattern == null) ? file.exists() : (pattern.matcher(file.getName()).matches()));
    }

    private static Pattern toPattern(final String regex0) {
        // file wildcard characters to regex characters...
        final String regex1 = regex0.replace(".", "\\.");
        final String regex2 = regex1.replace("*", ".*");
        final String regex3 = regex2.replace("?", ".");
        return Pattern.compile(regex3);
    }
}
