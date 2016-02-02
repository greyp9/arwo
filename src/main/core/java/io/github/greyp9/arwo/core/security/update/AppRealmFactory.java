package io.github.greyp9.arwo.core.security.update;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.security.realm.AppRealm;
import io.github.greyp9.arwo.core.security.realm.AuthPrincipal;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPathContext;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.validate.Validator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class AppRealmFactory {

    private AppRealmFactory() {
    }

    public static AppRealm toAppRealm(final String contextPath) {
        try {
            final byte[] xsd = StreamU.read(ResourceU.resolve(App.Realm.XSD));
            final File webappRoot = AppFolder.getWebappRoot(contextPath);
            final File realmFile = new File(webappRoot, "root/realm.xml");
            final byte[] xml = (realmFile.exists()) ? StreamU.read(realmFile) :
                    StreamU.read(ResourceU.resolve(App.Realm.XML_EMPTY));
            Logger.getLogger(AppRealmFactory.class.getName()).log(Level.OFF, realmFile.getPath());
            return AppRealmFactory.toAppRealm(xsd, xml);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    private static AppRealm toAppRealm(final byte[] xsd, final byte[] xml) throws IOException {
        final Validator validator = new Validator(xsd);
        final Collection<String> errors = validator.validate(xml);
        if (!errors.isEmpty()) {
            throw new IOException(errors.toString());
        }
        return toAppRealmValidated(DocumentU.toDocument(xml));
    }

    private static AppRealm toAppRealmValidated(final Document document) throws IOException {
        final URL url = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(url, null, null);
        final XPathContext context = xsdTypes.getContext();
        final XPather xpather = new XPather(document, context);
        final String name = xpather.getText("/realm:realm/@name");  // i18n xpath
        final Collection<AuthPrincipal> principals = new ArrayList<AuthPrincipal>();
        final List<Element> elements = xpather.getElements(
                "/realm:realm/realm:principals/realm:principal"); // i18n xpath
        for (final Element element : elements) {
            principals.add(toAuthPrincipal(element, context));
        }
        return new AppRealm(name, principals);
    }

    private static AuthPrincipal toAuthPrincipal(final Element element, final XPathContext context) throws IOException {
        final XPather xpather = new XPather(element, context);
        final String name = xpather.getText("realm:user");  // i18n xpath
        final String roles = xpather.getText("realm:roles");  // i18n xpath
        final String credential = xpather.getText("realm:credential");  // i18n xpath
        final AppPrincipal principal = new AppPrincipal(name, Collections.singleton(roles));
        return new AuthPrincipal(principal, credential);
    }
}
