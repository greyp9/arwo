package io.github.greyp9.arwo.core.xed.view;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import java.util.Collection;

public class XedTableView {
    private final String baseURI;
    private final XedCursor cursor;
    private final Collection<ViewInstance> viewInstances;

    public final String getBaseURI() {
        return baseURI;
    }

    public final XedCursor getCursor() {
        return cursor;
    }

    public final Collection<ViewInstance> getViewInstances() {
        return viewInstances;
    }

    public XedTableView(final String baseURI, final XedCursor cursor) {
        this.baseURI = baseURI;
        this.cursor = cursor;
        this.viewInstances = new ViewInstanceFactory(baseURI, cursor).getTableInstances();
    }

    public final String getItemNameI18n(final TypeInstance typeInstance, final TypeInstance pageInstance) {
        final String uri = cursor.getTypeInstance().getURI();
        final Bundle bundle = new Bundle(cursor.getXed().getBundle(uri));
        final String typeInstanceName = (typeInstance == null) ? null : typeInstance.getName();
        final String dataTypeName = (typeInstance == null) ? null :
                typeInstance.getDataType().getQName().getLocalPart();
        final String pageInstanceName = (pageInstance == null) ? null : pageInstance.getName();
        final String key = Value.join(".", typeInstanceName, dataTypeName, pageInstanceName);
        return bundle.getString(key, key);
    }
}
