package io.github.greyp9.arwo.core.alert;

public final class AlertU {

    private AlertU() {
    }

    public static Alert.Severity toInfoErr(final boolean b) {
        return (b ? Alert.Severity.INFO : Alert.Severity.ERR);
    }

    public static Alert.Severity toInfoWarn(final boolean b) {
        return (b ? Alert.Severity.INFO : Alert.Severity.WARN);
    }
}
