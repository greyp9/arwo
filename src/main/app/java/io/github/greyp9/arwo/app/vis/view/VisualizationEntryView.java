package io.github.greyp9.arwo.app.vis.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogramSerializer;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramDiv;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramText;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.Value;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Date;

public class VisualizationEntryView extends VisualizationView {
    private final TimeHistogram histogram;

    public VisualizationEntryView(ServletHttpRequest httpRequest, VisualizationRequest request,
                                  AppUserState userState, TimeHistogram histogram) {
        super(httpRequest, request, userState);
        this.histogram = histogram;
    }

    protected HttpResponse addContentTo(Element html) throws IOException {
        HttpResponse httpResponse = null;  // if data is found, return null
        if (histogram == null) {
            httpResponse = HttpResponseU.to404();
        } else if (Value.isEmpty(request.getPage())) {
            final Date date = DateU.floor(httpRequest.getDate(), histogram.getDurationPage());
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(),
                    request.getContext(), request.getMode(), DateX.toFilenameMM(date)));
        } else if (Value.isEmpty(request.getScale())) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI(),
                    request.getContext(), request.getMode(), request.getPage(), Integer.toString(2)));
        } else {
            addContentToSwitch(html);
        }
        return httpResponse;
    }

    private void addContentToSwitch(Element html) throws IOException {
        final Date date = DateU.floor(httpRequest.getDate(), histogram.getDurationPage());
        final Date dateQ = DateX.fromFilenameMM(request.getPage());
        if (date.equals(dateQ)) {
            // add content of active page
            addContentHistogram(html, histogram);
        } else {
            // add content of archived page
            final TimeHistogram histogramQ = new TimeHistogram(histogram.getName(), histogram.getFolder(), dateQ,
                    histogram.getDurationCell(), histogram.getDurationWord(), histogram.getDurationLine(),
                    histogram.getDurationParagraph(), histogram.getDurationPage(), histogram.getDurationPages());
            new TimeHistogramSerializer(histogramQ, new File(histogramQ.getFolder())).load(dateQ);
            addContentHistogram(html, histogramQ);
        }
    }

    protected void addContentHistogram(Element html, final TimeHistogram histogram)
            throws IOException {
        final String name = histogram.getName();
/*
        PropertiesX properties = new PropertiesX(userState.getPageVisualization().getProperties());
        long page = MathU.bound(0L, properties.getLong(name), histogram.getPageCount() - 1);
        properties.setLong(name, page);
        final String position = bundle.format("table.page.n.of.m", page + 1, histogram.getPageCount());
        final String scale = userState.getProperties().getProperty(httpRequest.getBaseURI());
        final int i = NumberU.toInt(scale, 2);
        // context menu
        final MenuView menuView = new MenuView(bundle, httpRequest, userState.getMenuSystem());
        menuView.addContentTo(html, AppMenuFactory.Const.NAV, name, false, true, null, position + " " + i);
*/
        // data view
        final int scale = NumberU.toInt(request.getScale(), 2);
        if (Html.TEXT.equals(request.getMode())) {
            new TimeHistogramText(histogram, name, 0).addContentTo(html);
        } else {
            new TimeHistogramDiv(histogram, name, 0, scale).addContentTo(html);
        }
    }
}
