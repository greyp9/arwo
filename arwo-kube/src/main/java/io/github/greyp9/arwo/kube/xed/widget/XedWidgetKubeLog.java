package io.github.greyp9.arwo.kube.xed.widget;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.widget.XedWidget;
import io.github.greyp9.arwo.core.xpath.XPather;

import java.io.IOException;
import java.util.Locale;
import java.util.Properties;

public final class XedWidgetKubeLog extends XedWidget {

    public XedWidgetKubeLog(final XedFactory factory, final Locale locale) throws IOException {
        super(App.Actions.QNAME_KUBE_LOG, factory, locale);
    }

    public void applyFrom(final Properties properties) throws IOException {
        getXedAction().update(NameTypeValuesU.create(
                "kubeLog.kubeLogType.pretty", properties.getProperty("kube.log.pretty"),
                "kubeLog.kubeLogType.previous", properties.getProperty("kube.log.previous"),
                "kubeLog.kubeLogType.tailLines", properties.getProperty("kube.log.tailLines"),
                "kubeLog.kubeLogType.timestamps", properties.getProperty("kube.log.timestamps")));
    }

    public void applyTo(final Properties properties) throws IOException {
        final XPather xpather = getXedAction().getXed().getXPather();
        properties.setProperty("kube.log.pretty", xpather.getText("/action:kubeLog/action:pretty"));
        properties.setProperty("kube.log.previous", xpather.getText("/action:kubeLog/action:previous"));
        properties.setProperty("kube.log.tailLines", xpather.getText("/action:kubeLog/action:tailLines"));
        properties.setProperty("kube.log.timestamps", xpather.getText("/action:kubeLog/action:timestamps"));
    }
}
