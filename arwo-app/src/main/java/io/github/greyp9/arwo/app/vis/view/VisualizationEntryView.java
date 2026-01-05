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
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramDiv;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramText;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogram;
import io.github.greyp9.arwo.core.metric.histogram2.time.TimeHistogramSerializer;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;

public final class VisualizationEntryView extends VisualizationView {
    private final TimeHistogram histogram;
    private final File folder;
    private final Properties initParams;

    public VisualizationEntryView(final ServletHttpRequest httpRequest, final VisualizationRequest request,
                                  final AppUserState userState, final TimeHistogram histogram) {
        super(httpRequest, request, userState);
        this.histogram = histogram;
        this.folder = new File(request.getHttpRequest().getInitParams().getProperty("folder"));
        this.initParams = request.getHttpRequest().getInitParams();
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

    protected MenuItem getMenuNav() {
        final Date date = DateX.fromFilenameMM(getRequest().getPage());
        final List<MenuItem> menuItems = new ArrayList<>();
        if (date != null) {
            menuItems.add(new MenuItem(getName(UTF16.ARROW_LEFT), null, App.Action.HREF_ABS, getHref(date, -1)));
            menuItems.add(new MenuItem(getName(UTF16.ARROW_RIGHT), null, App.Action.HREF_ABS, getHref(date, 1)));
        }
        return new MenuItem("Navigate", App.Target.USER_STATE, App.Action.MENU2, MENU_KEY, null, menuItems);
    }

    private String getName(final String text) {
        return Value.wrap("[", "]", Value.wrap(Html.SPACE, text));
    }

    private String getHref(final Date date, final long offset) {
        final VisualizationRequest request = getRequest();
        final Date dateOffset = histogram.incrementPage(date, (int) offset);
        final String pageTo = Value.defaultOnEmpty(DateX.toFilenameMM(dateOffset), "");
        return request.getHttpRequest().getURI().replace(request.getPage(), pageTo);
    }

    private static final String MENU_KEY = "/menu2/vis/nav/1";

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
        final NameTypeValues args = request.getArgs();
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
            new TimeHistogramText(histogramOp, label, dateOp, initParams, args).addContentTo(html);
        } else {
            new TimeHistogramDiv(histogramOp, label, dateOp, initParams, args, scale).addContentTo(html);
        }
    }
}
