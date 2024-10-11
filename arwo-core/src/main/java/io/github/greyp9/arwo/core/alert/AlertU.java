package io.github.greyp9.arwo.core.alert;

public final class AlertU {

    private AlertU() {
    }

    public static Alerts create(final Alert... alert) {
        final Alerts alerts = new Alerts();
        for (Alert alert1 : alert) {
            alerts.add(alert1);
        }
        return alerts;
    }

    public static Alert.Severity toInfoErr(final boolean b) {
        return (b ? Alert.Severity.INFO : Alert.Severity.ERR);
    }

    public static Alert.Severity toInfoWarn(final boolean b) {
        return (b ? Alert.Severity.INFO : Alert.Severity.WARN);
    }
}
