package io.github.greyp9.arwo.core.xed.test;

import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
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
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName nameRealm = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(nameRealm);
        final Xed xed = new Xed(document, xsdTypes);
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(nameRealm.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        final XPather xpather = new XPather(document, xsdTypes.getContext());
        final Element principals = xpather.getElement("/realm:realm/realm:principals");
        Assert.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, getValueInstancePrincipal1(typeInstancePrincipal));
        Assert.assertNotNull(principal1);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        final Element principal2 = xed.create(principals, getValueInstancePrincipal2(typeInstancePrincipal));
        Assert.assertNotNull(principal2);
        Assert.assertEquals(2, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
        Assert.assertEquals("arwo", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assert.assertEquals("arwo2", xpather.getText("/realm:realm/realm:principals/realm:principal[2]/realm:user"));
    }

    public void testUpdate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName nameRealm = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(nameRealm);
        final Xed xed = new Xed(document, xsdTypes);
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(nameRealm.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        final XPather xpather = new XPather(document, xsdTypes.getContext());
        final Element principals = xpather.getElement("/realm:realm/realm:principals");
        Assert.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, getValueInstancePrincipal1(typeInstancePrincipal));
        Assert.assertNotNull(principal1);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        final Element principal2 = xed.update(principal1, getValueInstancePrincipal2(typeInstancePrincipal));
        Assert.assertNotNull(principal2);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        Assert.assertEquals("arwo2", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assert.assertEquals("**", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:roles"));
        final Element principal3 = xed.update(principal2, getValueInstancePrincipal3(typeInstancePrincipal));
        Assert.assertNotNull(principal3);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
        Assert.assertEquals("arwo2", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:user"));
        Assert.assertEquals("***", xpather.getText("/realm:realm/realm:principals/realm:principal[1]/realm:roles"));
    }

    public void testDelete() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(SchemaSourceAppTest.Const.XSD_REALM);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final QName nameRealm = QNameU.getQName("{urn:arwo:realm}realm");
        final Document document = new DocumentFactory(typeDefinitions, false).generateEmpty(nameRealm);
        final Xed xed = new Xed(document, xsdTypes);
        final TypeInstance typeInstanceRealm = typeDefinitions.getElementTypes().get(nameRealm.toString());
        final TypeInstance typeInstancePrincipals = typeInstanceRealm.getInstance("principals");
        final TypeInstance typeInstancePrincipal = typeInstancePrincipals.getInstance("principal");
        final XPather xpather = new XPather(document, xsdTypes.getContext());
        final Element principals = xpather.getElement("/realm:realm/realm:principals");
        Assert.assertEquals(0, principals.getChildNodes().getLength());
        final Element principal1 = xed.create(principals, getValueInstancePrincipal1(typeInstancePrincipal));
        Assert.assertNotNull(principal1);
        Assert.assertEquals(1, principals.getChildNodes().getLength());
        final Element principalD = xed.delete(principal1);
        Assert.assertNotNull(principalD);
        Assert.assertEquals(0, principals.getChildNodes().getLength());
        logger.finest(DocumentU.toString(document));
    }

    private static ValueInstance getValueInstancePrincipal1(TypeInstance typeInstance) {
        final ValueInstance valueInstance = new ValueInstance(typeInstance);
        valueInstance.add(new NameTypeValue("user", null, "arwo"));
        valueInstance.add(new NameTypeValue("credential", null, "arwo"));
        valueInstance.add(new NameTypeValue("roles", null, "*"));
        return valueInstance;
    }

    private static ValueInstance getValueInstancePrincipal2(TypeInstance typeInstance) {
        final ValueInstance valueInstance = new ValueInstance(typeInstance);
        valueInstance.add(new NameTypeValue("user", null, "arwo2"));
        valueInstance.add(new NameTypeValue("credential", null, "arwo2"));
        valueInstance.add(new NameTypeValue("roles", null, "**"));
        return valueInstance;
    }

    private static ValueInstance getValueInstancePrincipal3(TypeInstance typeInstance) {
        final ValueInstance valueInstance = new ValueInstance(typeInstance);
        valueInstance.add(new NameTypeValue("roles", null, "***"));
        return valueInstance;
    }
}
