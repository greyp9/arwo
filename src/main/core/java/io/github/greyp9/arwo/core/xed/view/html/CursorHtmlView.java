package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.XedTableView;
import org.w3c.dom.Element;

import java.io.IOException;

public class CursorHtmlView extends HtmlView {
    private final XedCursorView cursorView;

    public CursorHtmlView(final XedRequest request, final XedCursorView cursorView) {
        super(request);
        this.cursorView = cursorView;
    }

    @Override
    public final void addContentTo(final Element html) throws IOException {
        final Object[] views = cursorView.getViews();
        for (final Object view : views) {
            if (view instanceof XedPropertyPageView) {
                addPropertyPage(html, (XedPropertyPageView) view);
            } else if (view instanceof XedTableView) {
                addTable(html, (XedTableView) view);
            }
        }
    }

    private void addPropertyPage(final Element html, final XedPropertyPageView view) throws IOException {
        final PropertyPageHtmlView htmlView = new PropertyPageHtmlView(view, getRequest());
        htmlView.addContentTo(html);
    }

    private void addTable(final Element html, final XedTableView view) throws IOException {
        final TableHtmlView htmlView = new TableHtmlView(view, getRequest());
        htmlView.addContentTo(html);
    }
}
