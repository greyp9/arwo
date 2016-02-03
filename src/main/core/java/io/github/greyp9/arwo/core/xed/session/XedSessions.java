package io.github.greyp9.arwo.core.xed.session;

import io.github.greyp9.arwo.core.xed.model.XedFactory;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

public class XedSessions {
    private final XedEntries entries;
    private final Map<String, XedSession> sessions;
    private final XedFactory factory;

    public XedSessions(final XedEntries entries, final XedFactory factory) {
        this.entries = entries;
        this.sessions = new TreeMap<String, XedSession>();
        this.factory = factory;
    }

    public final Collection<XedSession> getSessions() {
        return sessions.values();
    }

    public final XedSession getSession(final String contextPath) throws IOException {
        XedSession session = sessions.get(contextPath);
        if (session == null) {
            final XedEntry entry = entries.get(contextPath);
            if (entry != null) {
                session = new XedSessionFactory(entry, factory).create(entry.getQName());
                sessions.put(contextPath, session);
            }
        }
        return session;
    }

    public final XedSession removeSession(final String contextPath) throws IOException {
        return sessions.remove(contextPath);
    }

    public final void putSession(final String contextPath, final XedSession session) throws IOException {
        sessions.put(contextPath, session);
    }
}
