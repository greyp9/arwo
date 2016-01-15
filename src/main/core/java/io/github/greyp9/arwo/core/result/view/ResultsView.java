package io.github.greyp9.arwo.core.result.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.type.Result;
import io.github.greyp9.arwo.core.result.type.rowset.RowSetResult;
import io.github.greyp9.arwo.core.result.type.text.TextResult;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.text.render.TextRenderer;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

public class ResultsView {
    private final Results results;
    private final ResultsContext context;
    private final DateX dateX;

    public ResultsView(final Results results, final ResultsContext context) {
        this.results = results;
        this.context = context;
        this.dateX = context.getLocus().getDateX();
    }

    public final void addContentTo(final Element html) throws IOException {
        // setup
        final Date dateStart = results.getInterval().getDateStart();
        final Date dateFinish = results.getInterval().getDateFinish();
        final String command = results.getCommand();
        final Long elapsed = DurationU.toDuration(dateStart, dateFinish, new Date());
        // command
        final Element divCommand = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.COMMAND));
        renderHeader(divCommand, dateStart, command);
        renderBody(divCommand);
        renderFooter(divCommand, dateFinish, elapsed);
    }

    private void renderHeader(final Element html, final Date dateStart, final String command) throws IOException {
        final String dateText = ((dateStart == null) ? null : String.format("[@%s]", dateX.toString(dateStart)));
        final String stdinText = ((command == null) ? null : String.format("$ %s", command));
        final String text = Value.join(Html.SPACE, dateText, stdinText);
        ElementU.addElement(html, Html.DIV, text, NTV.create(Html.CLASS, App.CSS.COMMAND_HEAD));
    }

    private void renderBody(final Element html) throws IOException {
        final Element divBody = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.COMMAND_BODY));
        final Collection<Result> resultCollection = CollectionU.copy(new ArrayList<Result>(), results.getResults());
        for (final Result result : resultCollection) {
            if (result instanceof RowSetResult) {
                renderRowSet(divBody, (RowSetResult) result);
            } else if (result instanceof TextResult) {
                renderText(divBody, (TextResult) result);
            }
        }
        if (ElementU.isEmpty(divBody)) {
            ElementU.setTextContent(divBody, UTF16.NBSP);
        }
    }

    private void renderFooter(final Element html, final Date dateFinish, final Long elapsed) throws IOException {
        final String dateText = ((dateFinish == null) ? null : String.format("[@%s]", dateX.toString(dateFinish)));
        final String elapsedText = (elapsed == null) ? null : String.format("[%s]", DurationU.durationXSD(elapsed));
        final String text = Value.defaultOnEmpty(Value.join(Html.SPACE, dateText, elapsedText), UTF16.PAUSE);
        ElementU.addElement(html, Html.DIV, text, NTV.create(Html.CLASS, App.CSS.COMMAND_FOOT));
    }

    private void renderRowSet(final Element html, final RowSetResult result) throws IOException {
        final Element divR = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.ROWSET_RESULT));
        final RowSet rowSet = result.getRowSet();
        final RowSetMetaData metaData = rowSet.getMetaData();
        final Bundle bundle = context.getBundle();
        final Locus locus = context.getLocus();
        final ViewState viewState = context.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final TableContext tableContext = new TableContext(
                viewState, context.getSubmitID(), App.CSS.TABLE, bundle, locus);
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(divR);
    }

    private void renderText(final Element html, final TextResult result) throws IOException {
        // result wrapper
        final Element divR = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.TEXT_RESULT));
        // header
        if (result.getID() != null) {
            final String label = context.getBundle().getString(result.getID());
            ElementU.addElement(divR, Html.DIV, label, NTV.create(Html.CLASS, App.CSS.TEXT_RESULT_HEAD));
        }
        // body
        final TextRenderer textRenderer = new TextRenderer(result.getText());
        final String text = textRenderer.render(TextRenderer.Const.SCROLLBACK_LINES);
        final String htmlClass = Value.join(Html.SPACE, App.CSS.TEXT_RESULT_BODY, result.getType());
        final Element divTextBody = ElementU.addElement(divR, Html.DIV, null, NTV.create(Html.CLASS, htmlClass));
        toOutputText(divTextBody, text);
    }

    private void toOutputText(final Element div, final String text) {
        if (Value.isEmpty(text)) {
            ElementU.detach(div);
        } else {
            ElementU.addElement(div, Html.PRE, text);
        }
    }
}
