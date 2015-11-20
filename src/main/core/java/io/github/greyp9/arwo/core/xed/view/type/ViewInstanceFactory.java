package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xsd.core.XsdTypeU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;

import javax.xml.namespace.QName;
import java.util.ArrayList;
import java.util.Collection;

public class ViewInstanceFactory {
    private final String baseURI;
    private final XedCursor cursor;

    public ViewInstanceFactory(final String baseURI, final XedCursor cursor) {
        this.baseURI = baseURI;
        this.cursor = cursor;
    }

    public final Collection<ViewInstance> getPageInstances() {
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(typeInstance).getPageInstances();
        final Collection<ViewInstance> viewInstances = new ArrayList<ViewInstance>();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            viewInstances.add(toViewInstancePage(typeInstanceIt));
        }
        return viewInstances;
    }

    public final Collection<ViewInstance> getTableInstances() {
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(typeInstance).getTableInstances();
        final Collection<ViewInstance> viewInstances = new ArrayList<ViewInstance>();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            viewInstances.add(toViewInstanceTable(typeInstanceIt));
        }
        return viewInstances;
    }

    @SuppressWarnings({"PMD.CyclomaticComplexity", "PMD.StdCyclomaticComplexity",
            "PMD.ModifiedCyclomaticComplexity", "PMD.NPathComplexity", "PMD.ConfusingTernary"})
    private ViewInstance toViewInstancePage(final TypeInstance typeInstance) {
        ViewInstance viewInstance = null;
        final DataType dataType = typeInstance.getDataType();
        final QName qname = ((dataType == null) ? null : dataType.getQName());
        final boolean isMulti = (typeInstance.getMaxOccurs() > 1);
        final boolean isTextArea = (typeInstance.getDirective(Html.ROWS) != null);
        if (XsdTypeU.Const.BOOLEAN.equals(qname)) {
            viewInstance = new ViewInstanceBoolean(cursor, typeInstance);
        }
        if (isTextArea) {
            viewInstance = new ViewInstanceTextArea(cursor, typeInstance);
        }
        if (typeInstance instanceof ChoiceTypeInstance) {
            viewInstance = new ViewInstanceChoice(cursor, (ChoiceTypeInstance) typeInstance);
        }
        if (dataType != null) {
            if (!dataType.getInstances().isEmpty()) {  // is drill down
                viewInstance = new ViewInstanceDrillDown(baseURI, cursor, typeInstance);
            } else if (!dataType.getRestrictions().getEnumValues().isEmpty()) {
                viewInstance = new ViewInstanceEnum(cursor, typeInstance);
            }
        }
        if (isMulti) {
            viewInstance = new ViewInstanceDrillDown(baseURI, cursor, typeInstance);
        }
        if (viewInstance == null) {
            viewInstance = (typeInstance.isMasked() ?
                    new ViewInstanceTextMasked(cursor, typeInstance) :
                    new ViewInstanceText(cursor, typeInstance));
        }
        return viewInstance;
    }

    private ViewInstance toViewInstanceTable(final TypeInstance typeInstance) {
        return toViewInstancePage(typeInstance);
    }
}
