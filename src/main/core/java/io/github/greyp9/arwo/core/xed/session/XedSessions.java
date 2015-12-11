package io.github.greyp9.arwo.core.xed.session;

import io.github.greyp9.arwo.core.xed.model.Xed;

import java.io.IOException;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

public class XedSessions {
    private final XedEntries entries;
    private final Map<String, XedSession> sessions;

    public XedSessions(final XedEntries entries) {
        this.entries = entries;
        this.sessions = new TreeMap<String, XedSession>();
    }

    public final Collection<XedSession> getSessions() {
        return sessions.values();
    }

    public final XedSession getSession(final String contextPath, final Locale locale) throws IOException {
        XedSession session = sessions.get(contextPath);
        if (session == null) {
            final XedEntry entry = entries.get(contextPath);
            if (entry != null) {
                session = new XedSessionFactory(entry).create(entry.getQName(), locale);
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

    public final void applyLocale(final Locale locale) {
        for (final Map.Entry<String, XedSession> entry : sessions.entrySet()) {
            applyLocale(locale, entry.getKey(), entry.getValue());
        }
    }

    public final void applyLocale(final Locale locale, final String key, final XedSession session) {
        final Xed xed = session.getXed();
        final Xed xedUpdate = new Xed(xed.getDocument(), xed.getXsdTypes(), locale);
        final XedSession sessionUpdate = new XedSession(
                session.getEntry(), xedUpdate, session.getFile(), session.getDateLoad(), session.getTrigger());
        this.sessions.put(key, sessionUpdate);
    }
}
