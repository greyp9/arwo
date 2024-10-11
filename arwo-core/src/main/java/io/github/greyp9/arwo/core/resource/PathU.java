package io.github.greyp9.arwo.core.resource;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.value.Value;

public final class PathU {

    private PathU() {
    }

    public static String toPath(final String basePath, final String... tokens) {
        final StringBuilder buffer = new StringBuilder();
        buffer.append(basePath);
        for (final String token : tokens) {
            if (!Value.isEmpty(token)) {
                buffer.append(token.startsWith(Http.Token.SLASH) ? "" : Http.Token.SLASH);
                buffer.append(token);
            }
        }
        return buffer.toString();
    }

    public static String toDir(final String prefix, final String... tokens) {
        return toPath(prefix, tokens) + Http.Token.SLASH;
    }

    public static String toParent(final String path) {
        final boolean endsSlash = path.endsWith(Http.Token.SLASH);
        return path + (endsSlash ? "" : Http.Token.SLASH) + "..";
    }
}
