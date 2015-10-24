package io.github.greyp9.arwo.core.xed.view.text;

import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.XedTableView;

public class CursorTextView {
    private final XedCursorView cursorView;

    public CursorTextView(final XedCursorView cursorView) {
        this.cursorView = cursorView;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final String render() {
        final StringBuilder buffer = new StringBuilder();
        buffer.append(String.format("[%s]%n", cursorView.getCursor().getURI()));
        final Object[] views = cursorView.getViews();
        for (final Object view : views) {
            if (view instanceof XedPropertyPageView) {
                final PropertyPageTextView textView = new PropertyPageTextView((XedPropertyPageView) view);
                buffer.append(String.format("%s%n", textView.render()));
            } else if (view instanceof XedTableView) {
                final TableTextView textView = new TableTextView((XedTableView) view);
                buffer.append(String.format("%s%n", textView.render()));
            }
        }
        return buffer.toString();
    }
}
