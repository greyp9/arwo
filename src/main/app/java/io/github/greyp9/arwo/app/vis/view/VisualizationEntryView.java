package io.github.greyp9.arwo.app.vis.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.MathU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramDiv;
import io.github.greyp9.arwo.core.metric.histogram.view.TimeHistogramText;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.util.PropertiesX;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Map;
import java.util.Properties;

public class VisualizationEntryView extends VisualizationView {

    public VisualizationEntryView(ServletHttpRequest httpRequest, VisualizationRequest request, AppUserState userState) {
        super(httpRequest, request, userState);
    }

    protected HttpResponse addContentTo(Element html) throws IOException {
        final Properties initParams = httpRequest.getInitParams();
        for (Map.Entry<Object, Object> entry : initParams.entrySet()) {
            final String key = (String) entry.getKey();
            final String value = (String) entry.getValue();
            if ((key.equals(request.getContext())) && (TimeHistogram.class.getName().equals(value))) {
                final TimeHistogram histogram = (TimeHistogram) AppNaming.lookup(App.Application.LOOKUP, key);
                PropertiesX properties = new PropertiesX(userState.getPageVisualization().getProperties());
                long page = MathU.bound(0L, properties.getLong(key), histogram.getPageCount() - 1);
                properties.setLong(key, page);
                final String position = bundle.format("table.page.n.of.m", page + 1, histogram.getPageCount());
                final String scale = userState.getProperties().getProperty(httpRequest.getBaseURI());
                final int i = NumberU.toInt(scale, 2);
                // context menu
                final MenuView menuView = new MenuView(bundle, httpRequest, userState.getMenuSystem());
                menuView.addContentTo(html, AppMenuFactory.Const.NAV, key, false, true, null, position + " " + i);
                // data view
                if (Html.TEXT.equals(request.getMode())) {
                    new TimeHistogramText(histogram, key, (int) page).addContentTo(html);
                } else {
                    new TimeHistogramDiv(histogram, key, (int) page, i).addContentTo(html);
                }
            }
        }
        return null;
    }
}
