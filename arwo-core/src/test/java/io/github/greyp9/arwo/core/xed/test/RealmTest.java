package io.github.greyp9.arwo.core.xed.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.logging.Logger;

public class RealmTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testCreate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(qname.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        final XPather xpather = new XPather(document, xsdTypes.getContext());
        final Element principals = xpather.getElement("/realm:realm/realm:principals");
        Assertions.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo&credential=arwo&roles=*")));
        Assertions.assertNotNull(principal1);
        Assertions.assertEquals(1, principals.getChildNodes().getLength());
        final Element principal2 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**")));
        Assertions.assertNotNull(principal2);
        Assertions.assertEquals(2, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
        Assertions.assertEquals("arwo", xpather.getText(
                "/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assertions.assertEquals("arwo2", xpather.getText(
                "/realm:realm/realm:principals/realm:principal[2]/realm:user"));
    }

    @Test
    public void testUpdate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(qname.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        final XPather xpather = new XPather(document, xsdTypes.getContext());
        final Element principals = xpather.getElement("/realm:realm/realm:principals");
        Assertions.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo&credential=arwo&roles=*")));
        Assertions.assertNotNull(principal1);
        Assertions.assertEquals(1, principals.getChildNodes().getLength());
        final Element principal2 = xed.update(principal1, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**")));
        Assertions.assertNotNull(principal2);
        Assertions.assertEquals(1, principals.getChildNodes().getLength());
        Assertions.assertEquals("arwo2", xpather.getText(
                "/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assertions.assertEquals("**", xpather.getText(
                "/realm:realm/realm:principals/realm:principal[1]/realm:roles"));
        final Element principal3 = xed.update(principal2, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("roles=***")));
        Assertions.assertNotNull(principal3);
        Assertions.assertEquals(1, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
        Assertions.assertEquals("arwo2", xpather.getText(
                "/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assertions.assertEquals("***", xpather.getText(
                "/realm:realm/realm:principals/realm:principal[1]/realm:roles"));
    }

    @Test
    public void testDelete() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(qname.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        final XPather xpather = new XPather(document, xsdTypes.getContext());
        final Element principals = xpather.getElement("/realm:realm/realm:principals");
        Assertions.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo&credential=arwo&roles=*")));
        Assertions.assertNotNull(principal1);
        Assertions.assertEquals(1, principals.getChildNodes().getLength());
        final Element principalD = xed.delete(principal1);
        Assertions.assertNotNull(principalD);
        Assertions.assertEquals(0, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
    }
}
