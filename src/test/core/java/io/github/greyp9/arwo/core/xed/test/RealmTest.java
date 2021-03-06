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
import io.github.greyp9.arwo.core.xsd.source.test.SchemaSourceAppTest;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.logging.Logger;

public class RealmTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

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
        Assert.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo&credential=arwo&roles=*")));
        Assert.assertNotNull(principal1);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        final Element principal2 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**")));
        Assert.assertNotNull(principal2);
        Assert.assertEquals(2, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
        Assert.assertEquals("arwo", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assert.assertEquals("arwo2", xpather.getText("/realm:realm/realm:principals/realm:principal[2]/realm:user"));
    }

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
        Assert.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo&credential=arwo&roles=*")));
        Assert.assertNotNull(principal1);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        final Element principal2 = xed.update(principal1, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo2&credential=arwo2&roles=**")));
        Assert.assertNotNull(principal2);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        Assert.assertEquals("arwo2", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assert.assertEquals("**", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:roles"));
        final Element principal3 = xed.update(principal2, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("roles=***")));
        Assert.assertNotNull(principal3);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
        Assert.assertEquals("arwo2", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assert.assertEquals("***", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:roles"));
    }

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
        Assert.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, ValueInstance.create(typeInstancePrincipal,
                HttpArguments.toArguments("user=arwo&credential=arwo&roles=*")));
        Assert.assertNotNull(principal1);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        final Element principalD = xed.delete(principal1);
        Assert.assertNotNull(principalD);
        Assert.assertEquals(0, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
    }
}
