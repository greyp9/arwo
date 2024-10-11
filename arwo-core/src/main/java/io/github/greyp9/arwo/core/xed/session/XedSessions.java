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

    public final XedEntries getEntries() {
        return entries;
    }

    public final Collection<XedSession> getSessions() {
        return sessions.values();
    }

    public final XedSession getSession(final String contextPath) throws IOException {
        return getSession(contextPath, null);
    }

    public final XedSession getSession(final String contextPath, final XedEntry entryQ) throws IOException {
        final XedSession session = sessions.get(contextPath);
        return (session == null) ? createSession(contextPath, entryQ) : session;
    }

    private XedSession createSession(final String contextPath, final XedEntry entryQ) throws IOException {
        final XedEntry entryR = entries.get(contextPath);
        final boolean isUseQ = ((entryR == null) && (entryQ != null));
        final XedEntry entry = (isUseQ ? registerEntry(contextPath, entryQ) : entryR);
        return (entry == null) ? null : registerSession(contextPath, entry);
    }

    private XedEntry registerEntry(final String contextPath, final XedEntry entryQ) {
        final XedEntry entry = new XedEntry(contextPath, entryQ);
        entries.add(entry);
        return entry;
    }

    private XedSession registerSession(final String contextPath, final XedEntry entry) throws IOException {
        final XedSession session = new XedSessionFactory(entry, factory).create(entry.getQName());
        sessions.put(contextPath, session);
        return session;
    }

    public final XedSession removeSession(final String contextPath) throws IOException {
        return sessions.remove(contextPath);
    }

    public final void putSession(final String contextPath, final XedSession session) throws IOException {
        sessions.put(contextPath, session);
    }
}
