package io.github.greyp9.arwo.core.xed.session.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.session.XedEntries;
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessions;
import junit.framework.TestCase;
import org.junit.Assert;

import java.util.Locale;
import java.util.logging.Logger;

public class XedSessionsTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testEntries() throws Exception {
        XedEntry entryRealm = new XedEntry(null, "/users", App.Realm.QNAME, null, App.Realm.XSD, null);
        XedEntry entryLocale = new XedEntry(null, "/locale", App.Actions.QNAME_LOCALE, null, App.Actions.XSD, null);
        XedEntries entries = new XedEntries(entryRealm, entryLocale);
        logger.finest(entries.toString());
        Assert.assertEquals(2, entries.size());
        Assert.assertEquals(entryRealm, entries.get("/users"));
        Assert.assertEquals(entryLocale, entries.get("/locale"));
    }

    public void testSessions() throws Exception {
        final String xsdPathRealm = ResourceU.resolve(App.Realm.XSD).toExternalForm();
        final String xsdPathActions = ResourceU.resolve(App.Actions.XSD).toExternalForm();
        final XedEntry entryRealm = new XedEntry(null, "/users", App.Realm.QNAME, null, xsdPathRealm, null);
        final XedEntry entryFilter = new XedEntry(null, "/filter", App.Actions.QNAME_FILTER, null, xsdPathActions, null);
        final XedEntry entryLocale = new XedEntry(null, "/locale", App.Actions.QNAME_LOCALE, null, xsdPathActions, null);
        final XedEntries entries = new XedEntries(entryRealm, entryFilter, entryLocale);
        final XedSessions sessions = new XedSessions(entries);
        final Locale locale = Locale.getDefault();
        if (SystemU.isTrue()) {
            final XedSession sessionRealm = sessions.getSession("/users", locale);
            Assert.assertNotNull(sessionRealm);
            Assert.assertEquals(entryRealm, sessionRealm.getEntry());
        }
        if (SystemU.isTrue()) {
            final XedSession sessionFilter = sessions.getSession("/filter", locale);
            Assert.assertNotNull(sessionFilter);
            Assert.assertEquals(entryFilter, sessionFilter.getEntry());
        }
        if (SystemU.isTrue()) {
            final XedSession sessionLocale = sessions.getSession("/locale", locale);
            Assert.assertNotNull(sessionLocale);
            Assert.assertEquals(entryLocale, sessionLocale.getEntry());
        }
        if (SystemU.isTrue()) {
            final XedSession sessionNull = sessions.getSession("/null", locale);
            Assert.assertNull(sessionNull);
        }
    }
}
