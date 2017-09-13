package io.github.greyp9.arwo.app.vis.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.MathU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramDiv;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramText;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.util.PropertiesX;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Map;
import java.util.Properties;

public class VisualizationEntryView extends VisualizationView {

    public VisualizationEntryView(ServletHttpRequest httpRequest, VisualizationRequest request, AppUserState userState) {
        super(httpRequest, request, userState);
    }

    protected HttpResponse addContentTo(Element html) throws IOException {
        HttpResponse httpResponse;
        final NameTypeValue config = getConfigForContext(httpRequest.getInitParams());
        if (config == null) {
            httpResponse = HttpResponseU.to404();
        } else if (TimeHistogram.class.getName().equals(config.getValueS())) {
            httpResponse = addContentHistogram(html, config.getName());
        } else {
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }

    private NameTypeValue getConfigForContext(final Properties initParams) {
        NameTypeValue ntv = null;
        final String context = request.getContext();
        for (Map.Entry<Object, Object> entryIt : initParams.entrySet()) {
            final String key = (String) entryIt.getKey();
            if (key.equals(context)) {
                ntv = new NameTypeValue(key, entryIt.getValue());
                break;
            }
        }
        return ntv;
    }

    protected HttpResponse addContentHistogram(Element html, final String name) throws IOException {
        HttpResponse httpResponse = null;  // if data is found, return null
        final TimeHistogram histogram = (TimeHistogram) AppNaming.lookup(App.Application.LOOKUP, name);
        if (histogram == null) {
            httpResponse = HttpResponseU.to404();
        } else {
            PropertiesX properties = new PropertiesX(userState.getPageVisualization().getProperties());
            long page = MathU.bound(0L, properties.getLong(name), histogram.getPageCount() - 1);
            properties.setLong(name, page);
            final String position = bundle.format("table.page.n.of.m", page + 1, histogram.getPageCount());
            final String scale = userState.getProperties().getProperty(httpRequest.getBaseURI());
            final int i = NumberU.toInt(scale, 2);
            // context menu
            final MenuView menuView = new MenuView(bundle, httpRequest, userState.getMenuSystem());
            menuView.addContentTo(html, AppMenuFactory.Const.NAV, name, false, true, null, position + " " + i);
            // data view
            if (Html.TEXT.equals(request.getMode())) {
                new TimeHistogramText(histogram, name, (int) page).addContentTo(html);
            } else {
                new TimeHistogramDiv(histogram, name, (int) page, i).addContentTo(html);
            }
        }
        return httpResponse;
    }
}
