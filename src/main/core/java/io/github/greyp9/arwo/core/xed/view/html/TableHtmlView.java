package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.xed.view.XedTableView;
import org.w3c.dom.Element;

public class TableHtmlView {
    private final XedTableView view;

    public TableHtmlView(final XedTableView view) {
        this.view = view;
    }

    public final void addContentTo(final Element html) {
        view.getClass();
        html.getClass();
    }
}
