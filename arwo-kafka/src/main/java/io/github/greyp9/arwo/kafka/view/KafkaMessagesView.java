package io.github.greyp9.arwo.kafka.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedAction;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.kafka.client.ConsumeKafkaClient;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.header.Header;
import org.apache.kafka.common.header.Headers;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.sql.Types;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;

public class KafkaMessagesView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final ConsumeKafkaClient consumeKafkaClient;

    public KafkaMessagesView(final ServletHttpRequest httpRequest,
                             final AppUserState userState,
                             final ConsumeKafkaClient consumeKafkaClient) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.consumeKafkaClient = consumeKafkaClient;
    }

    public final HttpResponse toHttpResponse() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        new AlertsView(true, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                userState.getSubmitID()).addContentTo(body);
        ElementU.addElement(body, "h1", "Kafka");

        final String reference = consumeKafkaClient.getReference().get();
        final boolean isRunning = (reference == null);
        final Collection<ConsumerRecord<byte[], byte[]>> records = consumeKafkaClient.getRecords();
        final int count = records.size();
        final String summary = String.format("kafkaRunnable REFERENCE: %s - RUNNING: %s - COUNT: %d",
                reference, isRunning, count);
        ElementU.addElement(body, Html.DIV, summary, NTV.create());
        addContentTo(body, records);

        final XedAction action = new XedAction(App.Actions.QNAME_EMPTY, userState.getXedFactory(), null);
        final Xed xedUI = action.getXedUI(userState.getLocus().getLocale());
        final XedPropertyPageView pageView = new XedPropertyPageView(null, new XedNav(xedUI).getRoot());
        final ActionFactory factory = new ActionFactory(
                userState.getSubmitID(), xedUI.getBundle(), App.Target.SESSION, "empty", null);
        final Collection<String> actions = Arrays.asList("start", "stop", "clear");
        final ActionButtons buttons = factory.create("empty", false, actions);
        new PropertyStripHtmlView(pageView, buttons).addContentDiv(body);

        final AppTitle title = AppTitle.Factory.getResourceLabel(
                httpRequest, userState.getBundle(), "Kafka");
        final Preferences preferences = new Preferences(userState.getConfig());
        final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
        final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
        new AppHtml(httpRequest).fixup(html, title, iconColor, theme);

        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    public final void addContentTo(final Element html,
                                   final Collection<ConsumerRecord<byte[], byte[]>> records) throws IOException {
        final RowSet rowSet = loadRowSet(createMetaData(), records);
        final UserStateTable table = new UserStateTable(userState, null, httpRequest.getDate());
        table.toTableView(rowSet).addContentTo(html);
    }

    private RowSet loadRowSet(final RowSetMetaData metaData,
                              final Collection<ConsumerRecord<byte[], byte[]>> records) {
        final String baseURI = PathU.toPath(httpRequest.getBaseURI());
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (ConsumerRecord<byte[], byte[]> record : records) {
            loadRow(rowSet, baseURI, record);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(FIELD_SELECT, Types.VARCHAR, true),
                new ColumnMetaData(FIELD_TOPIC, Types.VARCHAR),
                new ColumnMetaData(FIELD_PARTITION, Types.INTEGER),
                new ColumnMetaData(FIELD_TIMESTAMP, Types.TIMESTAMP),
                new ColumnMetaData(FIELD_HEADERS, Types.VARCHAR),
                new ColumnMetaData(FIELD_KEY, Types.VARCHAR),
                new ColumnMetaData(FIELD_VALUE, Types.VARCHAR),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private void loadRow(final RowSet rowSet, final String baseURI, final ConsumerRecord<byte[], byte[]> record) {
        final String href = PathU.toDir(baseURI, Long.toString(record.offset()));
        final byte[] key = record.key();
        final byte[] value = record.value();
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, "select", href));
        insertRow.setNextColumn(record.topic());
        insertRow.setNextColumn(record.partition());
        insertRow.setNextColumn(XsdDateU.toXSDZMillis(new Date(record.timestamp())));
        insertRow.setNextColumn(toHeadersString(record.headers()));
        insertRow.setNextColumn(toEllipsis(key, href, "key"));
        insertRow.setNextColumn(toEllipsis(value, href, "value"));
        rowSet.add(insertRow.getRow());
    }

    private String toHeadersString(final Headers headers) {
        final StringBuilder buffer = new StringBuilder();
        for (Header header : headers) {
            buffer.append(String.format("[%s=%s]", header.key(), UTF8Codec.toString(header.value())));
        }
        return buffer.toString();
    }

    private Object toEllipsis(final byte[] bytes, final String baseURI, final String context) {
        final String string = Value.toStringEllipsis(UTF8Codec.toString(bytes), MAX_STRING_RENDER);
        return (string.length() > MAX_STRING_RENDER)
                ? new TableViewLink(string, null, PathU.toDir(baseURI, context)) : string;
    }

    private static final String FIELD_SELECT = "select";
    private static final String FIELD_TOPIC = "topic";
    private static final String FIELD_PARTITION = "partition";
    private static final String FIELD_TIMESTAMP = "timestamp";
    private static final String FIELD_HEADERS = "headers";
    private static final String FIELD_KEY = "key";
    private static final String FIELD_VALUE = "value";
    private static final String TABLE_ID = "kafkaMessageType";

    private static final int MAX_STRING_RENDER = 64;
}
