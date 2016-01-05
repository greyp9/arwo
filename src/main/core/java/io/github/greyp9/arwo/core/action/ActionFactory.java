package io.github.greyp9.arwo.core.action;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.value.Value;

import java.util.ArrayList;
import java.util.Collection;

public class ActionFactory {
    private final Bundle bundle;
    private final String submitID;
    private final String targetType;
    private final String cursorType;
    private final String uri;

    public ActionFactory(final String submitID, final Bundle bundle,
                         final String targetType, final String cursorType, final String uri) {
        this.submitID = submitID;
        this.bundle = bundle;
        this.targetType = targetType;
        this.cursorType = cursorType;
        this.uri = uri;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final ActionButtons create(
            final String actionType, final boolean isExpander, final Collection<String> actions) {
        final ArrayList<ActionButton> actionButtons = new ArrayList<ActionButton>();
        for (final String action : actions) {
            final String key = Value.join(Http.Token.DOT, actionType, Html.ACTION, action);
            final String label = bundle.getString(key, action);
            actionButtons.add(new ActionButton(label, null, targetType, action, cursorType, uri));
        }
        return new ActionButtons(submitID, bundle, isExpander, actionButtons);
    }
}
