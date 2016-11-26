package io.github.greyp9.arwo.core.http.header;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Host {
    private final String hostname;
    private final Integer port;

    public final String getHost() {
        return hostname;
    }

    public final Integer getPort() {
        return port;
    }

    public Host(final String header) {
        final Matcher matcher = Const.PATTERN.matcher(header);
        if (matcher.matches()) {
            this.hostname = matcher.group(Const.GROUP_HOST);
            final String portS = matcher.group(Const.GROUP_PORT);
            this.port = ((portS == null) ? null : Integer.parseInt(portS));
        } else {
            throw new IllegalArgumentException(header);
        }
    }

    @Override
    public final String toString() {
        return String.format("%s:%d", hostname, port);
    }

    private static class Const {
        private static final String REGEX = "(.+?)(:(\\d+))?";  // i18n internal
        private static final Pattern PATTERN = Pattern.compile(REGEX);
        private static final int GROUP_HOST = 1;
        private static final int GROUP_PORT = 3;
    }
}
