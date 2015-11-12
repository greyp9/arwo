package io.github.greyp9.arwo.core.xsd.value.test;

import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.transform.ValueInstanceTransform;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.logging.Logger;

public class ValueInstanceTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testTransform() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:enum1}file");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // validate
        final Collection<String> messages0 = xed.validate();
        Assert.assertEquals("[]", messages0.toString());
        // navigate in document
        final XedCursor cursorFile = new XedNav(xed).getRoot();
        final TypeInstance instanceFile = cursorFile.getTypeInstance();
        Assert.assertEquals(qname, instanceFile.getQName());
        // check
        final NameTypeValues nameTypeValues = NameTypeValuesU.create("name", "foo.txt", "type", "text");
        final ValueInstance valueInstance = ValueInstance.create(instanceFile, nameTypeValues);
        Assert.assertEquals(2, valueInstance.getNameTypeValues().size());
        // update value instance
        ValueInstance valueInstanceTransform = new ValueInstanceTransform().transform(valueInstance);
        // check
        final NameTypeValues nameTypeValuesTransform = valueInstanceTransform.getNameTypeValues();
        Assert.assertEquals(3, nameTypeValuesTransform.size());
        Assert.assertNull(nameTypeValuesTransform.getNameValue("name"));
        Assert.assertEquals("foo.txt", nameTypeValuesTransform.getValue("file.fileType.name"));
        Assert.assertEquals("text", nameTypeValuesTransform.getValue("file.fileType.type"));
        Assert.assertEquals("false", nameTypeValuesTransform.getValue("file.fileType.hidden"));
    }

    public static class Const {
        public static final String XSD = "io/github/greyp9/arwo/xsd/enum1/enum1.xsd";
    }
}
