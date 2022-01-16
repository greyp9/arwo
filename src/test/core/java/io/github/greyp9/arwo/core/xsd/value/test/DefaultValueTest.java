package io.github.greyp9.arwo.core.xsd.value.test;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.transform.DefaultTransform;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Logger;

public class DefaultValueTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testGenerateDefaultDocument() throws IOException {
        final URL urlInitial = ResourceU.resolve(App.Config.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final DocumentFactory documentFactory = new DocumentFactory(typeDefinitions);
        final QName qnameFavorites = QNameU.getQName("{urn:arwo:app}favorites");
        final Document document = documentFactory.generateEmpty(qnameFavorites);
        logger.finest(DocumentU.toString(document));
    }

    private TypeInstance getTypeInstance() throws IOException {
        final URL urlInitial = ResourceU.resolve(App.Config.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final TypeInstance favorites = typeDefinitions.getElementTypes().get("{urn:arwo:app}favorites");
        final TypeInstance lshFavorites = favorites.getInstance("lshFavorites");
        return lshFavorites.getInstance("lshFavorite");
    }

    @Test
    public void testGenerateDefaultAttribute() throws IOException {
        final TypeInstance lshFavorite = getTypeInstance();
        final TypeInstance enabled = lshFavorite.getInstance("enabled");
        final NameTypeValues ntv = NameTypeValuesU.create(
                "lshFavorite.lshFavoriteType.command", "ls",
                "lshFavorite.lshFavoriteType.comment", "comment");
        // no data for this attribute
        final ValueInstance valueInstance = ValueInstance.create(lshFavorite, ntv);
        Assert.assertEquals(2, valueInstance.getNameTypeValues().size());
        Assert.assertNull(valueInstance.getNameTypeValue(enabled));
        // default data populated for this attribute
        final ValueInstance valueInstanceWithDefault = new DefaultTransform().transform(valueInstance);
        Assert.assertEquals(ntv.size() + 1, valueInstanceWithDefault.getNameTypeValues().size());
        Assert.assertNotNull(valueInstanceWithDefault.getNameTypeValue(enabled));
        final NameTypeValue ntvEnabled = valueInstanceWithDefault.getNameTypeValue(enabled);
        Assert.assertEquals(Boolean.TRUE.toString(), ntvEnabled.getValueS());
    }

    @Test
    public void testGenerateDefaultAttributeEmpty() throws IOException {
        final TypeInstance lshFavorite = getTypeInstance();
        final TypeInstance enabled = lshFavorite.getInstance("enabled");
        // start with empty value instance
        final ValueInstance valueInstance = ValueInstance.create(lshFavorite, new NameTypeValues());
        Assert.assertEquals(0, valueInstance.getNameTypeValues().size());
        Assert.assertNull(valueInstance.getNameTypeValue(enabled));
        final ValueInstance valueInstanceWithDefault = new DefaultTransform().transform(valueInstance);
        Assert.assertEquals(1, valueInstanceWithDefault.getNameTypeValues().size());
        Assert.assertNotNull(valueInstanceWithDefault.getNameTypeValue(enabled));
        final NameTypeValue ntvEnabled = valueInstanceWithDefault.getNameTypeValue(enabled);
        Assert.assertEquals(Boolean.TRUE.toString(), ntvEnabled.getValueS());
    }

    @Test
    public void testOverrideDefaultAttribute() throws IOException {
        final TypeInstance lshFavorite = getTypeInstance();
        final TypeInstance enabled = lshFavorite.getInstance("enabled");
        final NameTypeValues ntv = NameTypeValuesU.create(
                "lshFavorite.lshFavoriteType.enabled", "false",
                "lshFavorite.lshFavoriteType.command", "ls",
                "lshFavorite.lshFavoriteType.comment", "comment");
        // no data for this attribute
        final ValueInstance valueInstance = ValueInstance.create(lshFavorite, ntv);
        Assert.assertEquals(ntv.size(), valueInstance.getNameTypeValues().size());
        // default data populated for this attribute
        final ValueInstance valueInstanceWithDefault = new DefaultTransform().transform(valueInstance);
        Assert.assertEquals(ntv.size(), valueInstanceWithDefault.getNameTypeValues().size());
        Assert.assertNotNull(valueInstanceWithDefault.getNameTypeValue(enabled));
        final NameTypeValue ntvEnabled = valueInstanceWithDefault.getNameTypeValue(enabled);
        Assert.assertEquals(Boolean.FALSE.toString(), ntvEnabled.getValueS());
    }
}
