package io.github.greyp9.arwo.core.xed.session.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.session.XedEntries;
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.logging.Logger;

public class XedSessionsTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testEntries() throws Exception {
        XedEntry entryRealm = new XedEntry(
                null, "/users", App.Realm.QNAME, null, App.Realm.XSD, null, null);
        XedEntry entryLocale = new XedEntry(
                null, "/locale", App.Actions.QNAME_LOCALE, null, App.Actions.XSD, null, null);
        XedEntries entries = new XedEntries(entryRealm, entryLocale);
        logger.finest(entries.toString());
        Assertions.assertEquals(2, entries.size());
        Assertions.assertEquals(entryRealm, entries.get("/users"));
        Assertions.assertEquals(entryLocale, entries.get("/locale"));
    }

    @Test
    public void testSessions() throws Exception {
        final String xsdPathRealm = ResourceU.resolve(App.Realm.XSD).toExternalForm();
        final String xsdPathActions = ResourceU.resolve(App.Actions.XSD).toExternalForm();
        final XedEntry entryRealm = new XedEntry(
                null, "/users", App.Realm.QNAME, null, xsdPathRealm, null, null);
        final XedEntry entryFilter = new XedEntry(
                null, "/filter", App.Actions.QNAME_FILTER, null, xsdPathActions, null, null);
        final XedEntry entryLocale = new XedEntry(
                null, "/locale", App.Actions.QNAME_LOCALE, null, xsdPathActions, null, null);
        final XedEntries entries = new XedEntries(entryRealm, entryFilter, entryLocale);
        final XedSessions sessions = new XedSessions(entries, new XedFactory());
        //final Locale locale = Locale.getDefault();
        if (SystemU.isTrue()) {
            final XedSession sessionRealm = sessions.getSession("/users");
            Assertions.assertNotNull(sessionRealm);
            Assertions.assertEquals(entryRealm, sessionRealm.getEntry());
        }
        if (SystemU.isTrue()) {
            final XedSession sessionFilter = sessions.getSession("/filter");
            Assertions.assertNotNull(sessionFilter);
            Assertions.assertEquals(entryFilter, sessionFilter.getEntry());
        }
        if (SystemU.isTrue()) {
            final XedSession sessionLocale = sessions.getSession("/locale");
            Assertions.assertNotNull(sessionLocale);
            Assertions.assertEquals(entryLocale, sessionLocale.getEntry());
        }
        if (SystemU.isTrue()) {
            final XedSession sessionNull = sessions.getSession("/null");
            Assertions.assertNull(sessionNull);
        }
    }
}
