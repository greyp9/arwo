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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;

import java.io.IOException;
import java.net.URL;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

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

    private TypeInstance getTypeInstanceFavoriteLSH() throws IOException {
        final URL urlInitial = ResourceU.resolve(App.Config.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final TypeInstance favorites = typeDefinitions.getElementTypes().get("{urn:arwo:app}favorites");
        final TypeInstance lshFavorites = favorites.getInstance("lshFavorites");
        return lshFavorites.getInstance("lshFavorite");
    }

    private TypeInstance getTypeInstanceLocalization() throws IOException {
        final URL urlInitial = ResourceU.resolve(App.Config.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final TypeDefinitions typeDefinitions = xsdTypes.getTypeDefinitions();
        final TypeInstance app = typeDefinitions.getElementTypes().get("{urn:arwo:app}app");
        final TypeInstance preferences = app.getInstance("preferences");
        return preferences.getInstance("localization");
    }

    @Test
    public void testGenerateDefaultAttribute() throws IOException {
        final TypeInstance localization = getTypeInstanceLocalization();
        final TypeInstance tz = localization.getInstance("tz");
        final NameTypeValues ntv = NameTypeValuesU.create(
                "localization.localizationType.language", "EN");
        // no data for this attribute
        final ValueInstance valueInstance = ValueInstance.create(localization, ntv);
        Assertions.assertEquals(1, valueInstance.getNameTypeValues().size());
        Assertions.assertNull(valueInstance.getNameTypeValue(tz));
        // default data populated for this attribute
        final ValueInstance valueInstanceWithDefault = new DefaultTransform().transform(valueInstance);
        Assertions.assertEquals(ntv.size() + 2, valueInstanceWithDefault.getNameTypeValues().size());
        Assertions.assertNotNull(valueInstanceWithDefault.getNameTypeValue(tz));
        final NameTypeValue ntvTZ = valueInstanceWithDefault.getNameTypeValue(tz);
        Assertions.assertEquals("GMT", ntvTZ.getValueS());
    }

    @Test
    public void testGenerateDefaultAttributeEmpty() throws IOException {
        final TypeInstance localization = getTypeInstanceLocalization();
        final TypeInstance tz = localization.getInstance("tz");
        // start with empty value instance
        final ValueInstance valueInstance = ValueInstance.create(localization, new NameTypeValues());
        Assertions.assertEquals(0, valueInstance.getNameTypeValues().size());
        Assertions.assertNull(valueInstance.getNameTypeValue(tz));
        final ValueInstance valueInstanceWithDefault = new DefaultTransform().transform(valueInstance);
        Assertions.assertEquals(2, valueInstanceWithDefault.getNameTypeValues().size());
        Assertions.assertNotNull(valueInstanceWithDefault.getNameTypeValue(tz));
        final NameTypeValue ntvTZ = valueInstanceWithDefault.getNameTypeValue(tz);
        Assertions.assertEquals("GMT", ntvTZ.getValueS());
    }

    @Test
    public void testOverrideDefaultAttribute() throws IOException {
        final TypeInstance lshFavorite = getTypeInstanceFavoriteLSH();
        final TypeInstance enabled = lshFavorite.getInstance("enabled");
        final NameTypeValues ntv = NameTypeValuesU.create(
                "lshFavorite.lshFavoriteType.enabled", "false",
                "lshFavorite.lshFavoriteType.command", "ls",
                "lshFavorite.lshFavoriteType.comment", "comment");
        // no data for this attribute
        final ValueInstance valueInstance = ValueInstance.create(lshFavorite, ntv);
        Assertions.assertEquals(ntv.size(), valueInstance.getNameTypeValues().size());
        // default data populated for this attribute
        final ValueInstance valueInstanceWithDefault = new DefaultTransform().transform(valueInstance);
        Assertions.assertEquals(ntv.size(), valueInstanceWithDefault.getNameTypeValues().size());
        Assertions.assertNotNull(valueInstanceWithDefault.getNameTypeValue(enabled));
        final NameTypeValue ntvEnabled = valueInstanceWithDefault.getNameTypeValue(enabled);
        Assertions.assertEquals(Boolean.FALSE.toString(), ntvEnabled.getValueS());
    }
}
