package io.github.greyp9.arwo.core.xsd.value.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.transform.ProtectHashTransform;
import io.github.greyp9.arwo.core.xed.transform.TransformContext;
import io.github.greyp9.arwo.core.xed.transform.ValueInstanceTransform;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.logging.Logger;

public class ValueInstanceTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testTransformNameForm() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_ENUM1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:enum1}file");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // validate
        final Collection<String> messages0 = xed.validate();
        Assertions.assertEquals("[]", messages0.toString());
        // navigate in document
        final XedCursor cursorFile = new XedNav(xed).getRoot();
        final TypeInstance instanceFile = cursorFile.getTypeInstance();
        Assertions.assertEquals(qname, instanceFile.getQName());
        // check
        final NameTypeValues nameTypeValues = NameTypeValuesU.create("name", "foo.txt", "type", "text");
        final ValueInstance valueInstance = ValueInstance.create(instanceFile, nameTypeValues);
        Assertions.assertEquals(2, valueInstance.getNameTypeValues().size());
        // update value instance
        ValueInstance valueInstanceTransform = new ValueInstanceTransform(null).transform(valueInstance);
        // check
        final NameTypeValues nameTypeValuesTransform = valueInstanceTransform.getNameTypeValues();
        Assertions.assertEquals(nameTypeValues.size() + 1, nameTypeValuesTransform.size());
        Assertions.assertNull(nameTypeValuesTransform.getNameValue("name"));
        Assertions.assertEquals("foo.txt", nameTypeValuesTransform.getValue("file.fileType.name"));
        Assertions.assertEquals("text", nameTypeValuesTransform.getValue("file.fileType.type"));
        Assertions.assertEquals("false", nameTypeValuesTransform.getValue("file.fileType.hidden"));
    }

    @Test
    public void testTransformProtectHash() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(App.Realm.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:realm}realm");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Element element = document.getDocumentElement();
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // navigate in document
        final XedCursor cursorRealm = new XedNav(xed).getRoot();
        final TypeInstance instanceRealm = cursorRealm.getTypeInstance();
        Assertions.assertEquals(qname, instanceRealm.getQName());
        final TypeInstance instancePrincipals = instanceRealm.getInstance("principals");
        Assertions.assertEquals(QNameU.getQName("{urn:arwo:realm}principals"), instancePrincipals.getQName());
        final TypeInstance instancePrincipal = instancePrincipals.getInstance("principal");
        Assertions.assertEquals(QNameU.getQName("{urn:arwo:realm}principal"), instancePrincipal.getQName());
        // check value instance transform
        if (SystemU.isTrue()) {
            final NameTypeValues ntv = NameTypeValuesU.create(
                    "principal.principalType.user", "appUser",
                    "principal.principalType.credential", "appCredential",
                    "principal.principalType.roles", "*");
            final ValueInstance valueInstance = ValueInstance.create(instancePrincipal, ntv);
            Assertions.assertEquals(ntv.size(), valueInstance.getNameTypeValues().size());
            final XPather xpather = new XPather(element, xed.getXPather().getContext());
            final TransformContext context = new TransformContext(null, xpather);
            final ValueInstance valueInstanceX = new ProtectHashTransform(valueInstance, context).transform();
            final String hash = Base64Codec.encode(HashU.sha256(UTF8Codec.toBytes("appCredential")));
            final NameTypeValues nameTypeValuesX = valueInstanceX.getNameTypeValues();
            Assertions.assertEquals(ntv.size(), nameTypeValuesX.size());
            Assertions.assertEquals("appUser", nameTypeValuesX.getValue("principal.principalType.user"));
            Assertions.assertEquals(hash, nameTypeValuesX.getValue("principal.principalType.credential"));
            Assertions.assertEquals("*", nameTypeValuesX.getValue("principal.principalType.roles"));
        }
        // check value instance transform
        if (SystemU.isTrue()) {
            final NameTypeValues ntv = NameTypeValuesU.create(
                    "principal.principalType.user", "appUser",
                    "principal.principalType.credential", Html.MASK,
                    "principal.principalType.roles", "*");
            final ValueInstance valueInstance = ValueInstance.create(instancePrincipal, ntv);
            Assertions.assertEquals(ntv.size(), valueInstance.getNameTypeValues().size());
            final XPather xpather = new XPather(element, xed.getXPather().getContext());
            final TransformContext context = new TransformContext(null, xpather);
            final ValueInstance valueInstanceX = new ProtectHashTransform(valueInstance, context).transform();
            final NameTypeValues nameTypeValuesX = valueInstanceX.getNameTypeValues();
            Assertions.assertEquals(2, nameTypeValuesX.size());
            Assertions.assertEquals("appUser", nameTypeValuesX.getValue("principal.principalType.user"));
            Assertions.assertEquals("*", nameTypeValuesX.getValue("principal.principalType.roles"));
        }
    }
}
