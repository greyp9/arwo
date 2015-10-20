package io.github.greyp9.arwo.core.resource;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Pather {
    private final String left;
    private final String right;

    public Pather(final String left, final String right) {
        this.left = left;
        this.right = right;
    }

    public Pather(final String path) {
        final Matcher matcher = ((path == null) ? null : Const.PATH_TOKEN.matcher(path));
        if ((matcher != null) && (matcher.matches())) {
            int i = 0;
            this.left = matcher.group(++i);
            this.right = matcher.group(++i);
        } else {
            this.left = path;
            this.right = null;
        }
    }

    public final String getLeft() {
        return left;
    }

    public final String getLeftToken() {
        return ((left == null) ? null : left.replace(Const.SLASH, ""));
    }

    public final String getRight() {
        return right;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public static String getParent(final String path) {
        final StringBuilder parent = new StringBuilder();
        Pather pather = new Pather(path);
        while (pather.getRight() != null) {
            if (!Const.SLASH.equals(pather.getRight())) {
                parent.append(pather.getLeft());
            }
            pather = new Pather(pather.getRight());
        }
        parent.append(Const.SLASH);
        return parent.toString();
    }

    private static class Const {
        private static final Pattern PATH_TOKEN = Pattern.compile("(/.*?)(/.*)?");
        private static final String SLASH = "/";
    }
}
