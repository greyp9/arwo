package io.github.greyp9.arwo.core.xed.session;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.security.update.RealmTrigger;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.security.Principal;
import java.util.Collection;

public class XedSessionsFactory {
    private final File webappRoot;

    public XedSessionsFactory(final File webappRoot) {
        this.webappRoot = webappRoot;
    }

    public final XedSessions getSessions(final Principal principal, final Locus locus) throws IOException {
        final XedEntries entries = new XedEntries();
        if (SystemU.isTrue()) {
            addEntryRealm(entries);
        }
        addEntryFavorites(entries, principal);
        addEntryConfig(entries, principal);
        addEntriesConfig(entries, principal, locus);
        return new XedSessions(entries);
    }

    private void addEntryRealm(final XedEntries entries) throws IOException {
        final File realmFile = new File(webappRoot, "root/realm.xml");
        final String xmlPath = realmFile.getCanonicalPath();
        final String xsdPath = ResourceU.resolve(App.Realm.XSD).toExternalForm();
        final String trigger = RealmTrigger.class.getName();
        entries.add(new XedEntry(null, App.Servlet.USERS, App.Realm.QNAME, xmlPath, xsdPath, null, trigger));
    }

    // user favorites (bookmarks)
    private void addEntryFavorites(final XedEntries entries, final Principal principal) throws IOException {
        final File userHome = AppFolder.getUserHome(webappRoot, principal);
        final File fileConfig = new File(userHome, "config/fav.xml");
        final String xmlPath = fileConfig.getCanonicalPath();
        final String xsdPath = ResourceU.resolve(App.Config.XSD).toExternalForm();
        entries.add(new XedEntry(null, App.Servlet.FAVORITES, App.Config.QNAME_FAVS, xmlPath, xsdPath, null, null));
    }

    // user settings
    private void addEntryConfig(final XedEntries entries, final Principal principal) throws IOException {
        final File userHome = AppFolder.getUserHome(webappRoot, principal);
        final File fileConfig = new File(userHome, "config/app.xml");
        final String xmlPath = fileConfig.getCanonicalPath();
        final String xsdPath = ResourceU.resolve(App.Config.XSD).toExternalForm();
        entries.add(new XedEntry(null, App.Servlet.SETTINGS, App.Config.QNAME_APP, xmlPath, xsdPath, null, null));
    }

    // documents defined in user settings
    private void addEntriesConfig(
            final XedEntries entries, final Principal principal, final Locus locus) throws IOException {
        final File userHome = AppFolder.getUserHome(webappRoot, principal);
        final File fileConfig = new File(userHome, "config/app.xml");
        if (fileConfig.exists()) {
            // load document model
            final URL urlAppXSD = ResourceU.resolve(App.Config.XSD);
            final XsdTypes xsdTypes = new XsdTypes(urlAppXSD);
            final Document document = DocumentU.toDocument(StreamU.read(fileConfig));
            final Xed xed = new Xed(document, xsdTypes, locus.getLocale());
            // find configuration
            final XPather xpather = xed.getXPather();
            final XedNav nav = new XedNav(xed);
            final XedCursor cursorDocuments = nav.find(xpather.getElement("/app:app/app:documents"));  // i18n xpath
            final TypeInstance typeDocument = cursorDocuments.getTypeInstance().getInstance("document");  // i18n xpath
            final Collection<Element> elements = cursorDocuments.getChildren(typeDocument);
            for (final Element element : elements) {
                // load entry
                final XedCursor cursorDocument = nav.find(element, cursorDocuments);
                addEntry(entries, typeDocument, cursorDocument);
            }
        }
    }

    private void addEntry(final XedEntries entries, final TypeInstance typeDocument, final XedCursor cursorDocument) {
        final String title = cursorDocument.getValue(typeDocument.getInstance("title"));  // i18n xpath
        final String context = cursorDocument.getValue(typeDocument.getInstance("contextPath"));  // i18n xpath
        final String qname = cursorDocument.getValue(typeDocument.getInstance("qname"));  // i18n xpath
        final String xmlPath = cursorDocument.getValue(typeDocument.getInstance("xmlPath"));  // i18n xpath
        final String xsdPath = cursorDocument.getValue(typeDocument.getInstance("xsdPath"));  // i18n xpath
        final String xsltPath = cursorDocument.getValue(typeDocument.getInstance("xsltPath"));  // i18n xpath
        entries.add(new XedEntry(title, context, qname, xmlPath, xsdPath, xsltPath, null));
    }
}
