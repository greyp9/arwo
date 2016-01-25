package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import java.util.Collection;

public class ViewInstanceChoice extends ViewInstance {
    private final ChoiceTypeInstance choiceInstance;

    public final ChoiceTypeInstance getChoiceInstance() {
        return choiceInstance;
    }

    public ViewInstanceChoice(final XedCursor cursor, final ChoiceTypeInstance typeInstance) {
        super(cursor, typeInstance);
        this.choiceInstance = typeInstance;
    }

    public final String getValue() {
        final StringBuilder buffer = new StringBuilder();
        final String value = getCursor().getValue(getTypeInstance());
        final Collection<TypeInstance> typeInstances = choiceInstance.getInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            final boolean isSelected = typeInstanceIt.getName().equals(value);
            final String textSelected = (isSelected ? "x" : " ");  // i18n internal
            buffer.append(String.format("[%s] %s   ", textSelected, typeInstanceIt.getName()));
        }
        return buffer.toString();
    }
}
