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
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Date;

public class ResultsView {
    private final Results results;
    private final ResultsContext context;
    private final DateX dateX;

    public ResultsView(Results results, ResultsContext context) {
        this.results = results;
        this.context = context;
        this.dateX = context.getLocus().getDateX();
    }

    public final void addContentTo(final Element html) throws IOException {
        // setup
        Date dateStart = results.getInterval().getDateStart();
        Date dateFinish = results.getInterval().getDateFinish();
        String command = results.getCommand();
        Long elapsed = DurationU.toDuration(dateStart, dateFinish, new Date());
        // command
        Element divCommand = ElementU.addElement(html, Html.DIV, null,
                NameTypeValuesU.create(Html.CLASS, App.CSS.COMMAND));
        renderHeader(divCommand, dateStart, command);
        renderBody(divCommand);
        renderFooter(divCommand, dateFinish, elapsed);
    }

    private void renderHeader(Element html, Date dateStart, String command) throws IOException {
        String dateText = ((dateStart == null) ? null : String.format("[@%s]", dateX.toString(dateStart)));
        String stdinText = ((command == null) ? null : String.format("$ %s", command));
        String text = Value.join(Html.SPACE, dateText, stdinText);
        ElementU.addElement(html, Html.DIV, text, NTV.create(Html.CLASS, App.CSS.COMMAND_HEAD));
    }

    private void renderBody(Element html) throws IOException {
        Element divBody = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.COMMAND_BODY));
        for (Result result : results.getResults()) {
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

    private void renderFooter(Element html, Date dateFinish, Long elapsed) throws IOException {
        String dateText = ((dateFinish == null) ? null : String.format("[@%s]", dateX.toString(dateFinish)));
        String elapsedText = (elapsed == null) ? null : String.format("[%s]", DurationU.durationXSD(elapsed));
        String text = Value.defaultOnEmpty(Value.join(Html.SPACE, dateText, elapsedText), UTF16.PAUSE);
        ElementU.addElement(html, Html.DIV, text, NTV.create(Html.CLASS, App.CSS.COMMAND_FOOT));
    }

    private void renderRowSet(Element html, RowSetResult result) throws IOException {
        RowSet rowSet = result.getRowSet();
        RowSetMetaData metaData = rowSet.getMetaData();
        final Bundle bundle = context.getBundle();
        final Locus locus = context.getLocus();
        ViewState viewState = context.getViewStates().getViewState(metaData, bundle, locus);
        Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        TableContext tableContext = new TableContext(viewState, context.getSubmitID(), App.CSS.TABLE, bundle, locus);
        TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }

    private void renderText(Element html, TextResult result) throws IOException {
        TextRenderer textRenderer = new TextRenderer(result.getText());
        String htmlClass = Value.join(Html.SPACE, App.CSS.TEXT_RESULT, result.getType());
        Element divResult = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, htmlClass));
        if (result.getID() != null) {
            String label = context.getBundle().getString(result.getID());
            ElementU.addElement(divResult, Html.DIV, label, NTV.create(Html.CLASS, App.CSS.TEXT_RESULT_HEAD));
        }
        toOutputText(divResult, textRenderer.render(TextRenderer.Const.SCROLLBACK_LINES));
    }

    private void toOutputText(Element div, String text) {
        if (Value.isEmpty(text)) {
            ElementU.detach(div);
        } else {
            ElementU.addElement(div, Html.PRE, text);
        }
    }
}
