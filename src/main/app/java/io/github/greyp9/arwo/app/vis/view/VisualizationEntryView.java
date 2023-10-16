package io.github.greyp9.arwo.app.vis.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramDiv;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramText;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogram;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogramSerializer;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.TimeZone;

public final class VisualizationEntryView extends VisualizationView {
    private final TimeHistogram histogram;
    private final File folder;
    private final TimeZone timeZone;

    public VisualizationEntryView(final ServletHttpRequest httpRequest, final VisualizationRequest request,
                                  final AppUserState userState, final TimeHistogram histogram,
                                  final File folder, final TimeZone timeZone) {
        super(httpRequest, request, userState);
        this.histogram = histogram;
        this.folder = folder;
        this.timeZone = timeZone;
    }

    protected HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getHttpRequest();
        final VisualizationRequest request = getRequest();
        HttpResponse httpResponse = null;  // if data is found, return null
        if (histogram == null) {
            httpResponse = HttpResponseU.to404();
        } else if (Value.isEmpty(request.getPage())) {
            final Date dateFloor = DateU.floor(httpRequest.getDate(),
                    histogram.getDurationPage(), XsdDateU.fromXSDZ(histogram.getEpoch()));
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), request.getContext(),
                    request.getMetric(), request.getMode(), DateX.toFilenameMM(dateFloor)));
        } else if (Value.isEmpty(request.getScale())) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(), request.getContext(),
                    request.getMetric(), request.getMode(), request.getPage(), Integer.toString(2)));
        } else {
            addContentToSwitch(html);
        }
        return httpResponse;
    }

    protected void addMenuNav(final Element html) {
        final VisualizationRequest request = getRequest();
        final Date date = DateX.fromFilenameMM(request.getPage());
        if (date != null) {
            final Element divMenu = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
            ElementU.addElement(divMenu, Html.SPAN, "[Navigate]", NTV.create(Html.CLASS, App.CSS.MENU));
            addMenuEntryNav(divMenu, UTF16.ARROW_LEFT, date, -1);
            addMenuEntryNav(divMenu, UTF16.ARROW_RIGHT, date, 1);
        }
    }

    private void addMenuEntryNav(final Element divMenu, final String text, final Date date, final long offset) {
        final VisualizationRequest request = getRequest();
        final String textPad = Value.wrap("[", "]", Value.wrap(Html.SPACE, text));
        final Date dateOffset = histogram.incrementPage(date, (int) offset);
        final String pageTo = Value.defaultOnEmpty(DateX.toFilenameMM(dateOffset), "");
        final String hrefTo = request.getHttpRequest().getURI().replace(request.getPage(), pageTo);
        final String cssClass = Value.join(Html.SPACE, App.CSS.MENU, App.CSS.MIN);
        ElementU.addElement(divMenu, Html.A, textPad, NTV.create(Html.CLASS, cssClass, Html.HREF, hrefTo));
    }

    private void addContentToSwitch(final Element html) throws IOException {
        final VisualizationRequest request = getRequest();
        final Date dateQ = DateX.fromFilenameMM(request.getPage());
        final TimeHistogram histogramQ = new TimeHistogram(histogram.getName(), request.getMetric(),
                histogram.getDurationCell(), histogram.getDurationPage(), histogram.getEpoch());
        new TimeHistogramSerializer(folder, histogramQ).loadAll();
        addContentHistogram(html, histogramQ, dateQ);
    }

    private void addContentHistogram(final Element html, final TimeHistogram histogramOp, final Date dateOp) {
        final VisualizationRequest request = getRequest();
        final String label = request.getMetric();
        final float scale = NumberU.toFloat(request.getScale(), 2.0f);
/*
        PropertiesX properties = new PropertiesX(userState.getPageVisualization().getProperties());
        long page = MathU.bound(0L, properties.getLong(name), histogram.getPageCount() - 1);
        properties.setLong(name, page);
        final String position = bundle.format("table.page.n.of.m", page + 1, histogram.getPageCount());
        final String scale = userState.getProperties().getProperty(httpRequest.getBaseURI());
        final int i = NumberU.toInt(scale, 2);
        // context menu
        final MenuView menuView = new MenuView(getBundle(), getHttpRequest(), getUserState().getMenuSystem());
        menuView.addContentTo(html, AppMenuFactory.Const.NAV, name, false, true, null, position + " " + i);
*/
        // data view
        if (Html.TEXT.equals(request.getMode())) {
            new TimeHistogramText(histogramOp, label, dateOp, timeZone).addContentTo(html);
        } else {
            new TimeHistogramDiv(histogramOp, label, dateOp, timeZone, scale).addContentTo(html);
        }
    }
}
