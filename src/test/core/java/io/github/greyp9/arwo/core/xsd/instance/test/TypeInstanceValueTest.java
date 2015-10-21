package io.github.greyp9.arwo.core.xsd.instance.test;

import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.document.TypeInstanceFactory;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollectionFactory;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitionsFactory;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;
import io.github.greyp9.arwo.core.xsd.type.TypeComponentsFactory;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

public class TypeInstanceValueTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testWebApp() throws Exception {
        File fileWebApp25 = new File(SystemU.resolve("~/Downloads/xsd/JavaEE5/web-app_2_5.xsd"));
        if (fileWebApp25.exists()) {
            URL urlInitial = URLCodec.toURL(fileWebApp25);
            URL urlCatalog = URLCodec.resolve(urlInitial, ".");
            // resolve schema collection for this initial schema
            SchemaCollectionFactory schemaCollectionFactory = new SchemaCollectionFactory(urlCatalog, null);
            SchemaCollection schemaCollection = schemaCollectionFactory.create(urlInitial);
            // pre-parse of schema collection
            TypeComponentsFactory typeComponentsFactory = new TypeComponentsFactory(schemaCollection);
            TypeComponents typeComponents = typeComponentsFactory.create();
            // create type definitions for schema set
            TypeDefinitionsFactory typeDefinitionsFactory = new TypeDefinitionsFactory(typeComponents);
            TypeDefinitions typeDefinitions = typeDefinitionsFactory.create();
            // check some type instances
            final String nameWebApp = "{http://java.sun.com/xml/ns/javaee}web-app";
            final TypeInstance typeInstanceWebApp = typeDefinitions.getElementTypes().get(nameWebApp);
            Assert.assertNotNull(typeInstanceWebApp);
            final TypeInstance typeInstanceChoice = typeInstanceWebApp.getDataType().getInstances().iterator().next();
            Assert.assertTrue(typeInstanceChoice instanceof ChoiceTypeInstance);
            ChoiceTypeInstance choiceTypeInstance = (ChoiceTypeInstance) typeInstanceChoice;
            // filter
            final TypeInstance typeInstanceFilter = choiceTypeInstance.getInstance("filter");
            Assert.assertNotNull(typeInstanceFilter);
            doTestFilter(typeInstanceFilter, typeDefinitions);
            // filter-mapping
            final TypeInstance typeInstanceFilterMapping = choiceTypeInstance.getInstance("filter-mapping");
            Assert.assertNotNull(typeInstanceFilterMapping);
            doTestFilterMapping(typeInstanceFilterMapping, typeDefinitions);
        }
    }

    private void doTestFilter(TypeInstance typeInstanceFilter, TypeDefinitions typeDefinitions) throws IOException {
        // testing handling of base types
        final TypeInstance typeInstance = typeInstanceFilter.getInstance("filter-name");
        Assert.assertNotNull(typeInstance);
        final DataType dataType1 = typeInstance.getDataType();
        Assert.assertEquals("{http://java.sun.com/xml/ns/javaee}filter-nameType", dataType1.getQName().toString());
        final DataType dataType2 = dataType1.getBaseType();
        Assert.assertEquals("{http://java.sun.com/xml/ns/javaee}nonEmptyStringType", dataType2.getQName().toString());
        final DataType dataType3 = dataType2.getBaseType();
        Assert.assertEquals("{http://java.sun.com/xml/ns/javaee}string", dataType3.getQName().toString());
        final DataType dataType4 = dataType3.getBaseType();
        Assert.assertEquals("{http://www.w3.org/2001/XMLSchema}token", dataType4.getQName().toString());
        final DataType dataType5 = dataType4.getBaseType();
        Assert.assertNull(dataType5);
        // testing handling of base types
        final DocumentFactory documentFactory = new DocumentFactory(typeDefinitions, true);
        final TypeInstanceFactory instanceFactory = documentFactory.getInstanceFactory();
        Assert.assertEquals("", instanceFactory.getDefaultValue(dataType4));
        Assert.assertEquals("", instanceFactory.getDefaultValue(dataType3));
        Assert.assertEquals("X", instanceFactory.getDefaultValue(dataType2));
        Assert.assertEquals("X", instanceFactory.getDefaultValue(dataType1));
        Assert.assertEquals("X", instanceFactory.getDefaultValue(typeInstance));
        // testing handling of base types
        final Document document = documentFactory.generateEmpty(typeInstance.getQName(), typeInstance);
        logger.log(Level.FINEST, DocumentU.toString(document));
    }

    private void doTestFilterMapping(TypeInstance typeInstanceFM, TypeDefinitions typeDefinitions) throws IOException {
        // testing handling of base types
        final TypeInstance typeInstance = typeInstanceFM.getInstance("dispatcher");
        Assert.assertNotNull(typeInstance);
        final DataType dataType1 = typeInstance.getDataType();
        Assert.assertEquals("{http://java.sun.com/xml/ns/javaee}dispatcherType", dataType1.getQName().toString());
        final DataType dataType2 = dataType1.getBaseType();
        Assert.assertEquals("{http://java.sun.com/xml/ns/javaee}string", dataType2.getQName().toString());
        final DataType dataType3 = dataType2.getBaseType();
        Assert.assertEquals("{http://www.w3.org/2001/XMLSchema}token", dataType3.getQName().toString());
        final DataType dataType4 = dataType3.getBaseType();
        Assert.assertNull(dataType4);
        // testing handling of base types
        final DocumentFactory documentFactory = new DocumentFactory(typeDefinitions, true);
        final TypeInstanceFactory instanceFactory = documentFactory.getInstanceFactory();
        Assert.assertEquals("", instanceFactory.getDefaultValue(dataType3));
        Assert.assertEquals("", instanceFactory.getDefaultValue(dataType2));
        Assert.assertEquals("FORWARD", instanceFactory.getDefaultValue(dataType1));
        Assert.assertEquals("FORWARD", instanceFactory.getDefaultValue(typeInstance));
        // testing handling of base types
        final Document document = documentFactory.generateEmpty(typeInstance.getQName(), typeInstance);
        logger.log(Level.FINEST, DocumentU.toString(document));
    }
}
