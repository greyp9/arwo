package io.github.greyp9.arwo.core.meter;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.table.XedMetaDataFactory;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;

public final class Meter {
    private final Xed xed;

    public Xed getXed() {
        return xed;
    }

    public Meter(final QName qname, final XedFactory factory) throws IOException {
        final URL url = ResourceU.resolve(App.Meter.XSD);
        //final XsdTypes xsdTypes = new XsdTypes(url, null, null);
        final XsdTypes xsdTypes = factory.getXsdTypes(url, null, null);
        final DocumentFactory documentFactory = new DocumentFactory(xsdTypes.getTypeDefinitions());
        final Document document = documentFactory.generateEmpty(qname);
        this.xed = new Xed(document, xsdTypes);
    }

    public RowSet getRowSet(final String qnameDataType, final String qnameTypeInstance) {
        // "{urn:arwo:meter}cronJobsType" / "job"
        final DataType dataType = xed.getXsdTypes().getTypeDefinitions().getComplexTypes().get(qnameDataType);
        final TypeInstance typeInstance = dataType.getInstance(qnameTypeInstance);
        final RowSetMetaData metaData = new XedMetaDataFactory().create(typeInstance, false);
        return new RowSet(metaData, null, null);
    }
}
