package io.github.greyp9.arwo.core.xed.state;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.locus.LocusFactory;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.menu.XedMenuFactory;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessions;
import io.github.greyp9.arwo.core.xed.session.XedSessionsFactory;
import io.github.greyp9.arwo.core.xed.session.action.SessionReload;
import io.github.greyp9.arwo.core.xed.session.action.SessionValidate;

import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.Properties;

public class XedUserState {
    // user session identity token
    private final String submitID;
    // table view states
    private final ViewStates viewStates;
    // user alerts
    private final Alerts alerts;
    // open document sessions
    private final XedSessions sessions;
    // session properties
    private final Properties properties;
    // menu system
    private final MenuSystem menuSystem;
    // document content clipboard
    private final XedClipboard clipboard;
    // user region preferences
    private Locus locus;

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public final XedSessions getSessions() {
        return sessions;
    }

    public final XedSession getSession(final String contextPath) throws IOException {
        return sessions.getSession(contextPath, locus.getLocale());
    }

    public final Properties getProperties() {
        return properties;
    }

    public final MenuSystem getMenuSystem() {
        return menuSystem;
    }

    public final XedClipboard getClipboard() {
        return clipboard;
    }

    public final Locus getLocus() {
        return locus;
    }

    public XedUserState(final File webappRoot, final Principal principal, final String submitID, final Locus locus)
            throws IOException {
        this.submitID = submitID;
        this.viewStates = new ViewStates();
        this.alerts = new Alerts();
        this.sessions = new XedSessionsFactory(webappRoot).getSessions(principal, locus);
        this.locus = locus;
        this.properties = new Properties();
        this.menuSystem = new MenuSystem(submitID, new XedMenuFactory());
        this.clipboard = new XedClipboard();
    }

    public final void apply(final NameTypeValues nameTypeValues) throws IOException {
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            if ("toggle".equals(nameTypeValue.getName())) {
                final boolean isExpanded = Boolean.parseBoolean(properties.getProperty("buttons"));
                properties.setProperty("buttons", Boolean.toString(!isExpanded));
            }
        }
    }

    public final String apply(final SubmitToken token, final NameTypeValues nameTypeValues,
                              final XedRequest request) throws IOException {
        String location = request.getHttpRequest().getURI();
        final String action = token.getAction();
        final String object = token.getObject();
        final String message = request.getBundle().getString("alert.action.not.implemented");
        if ("locale".equals(object)) {
            applyLocale(nameTypeValues);
        } else if ("toggle".equals(action)) {
            menuSystem.toggle(token.getObject());
        } else if ("xml".equals(action)) {
            location = toView(request.getHttpRequest(), action);
        } else if ("xsd".equals(action)) {
            alerts.add(new Alert(Alert.Severity.WARN, message));
        } else if ("type".equals(action)) {
            alerts.add(new Alert(Alert.Severity.WARN, message));
        } else if ("rev".equals(action)) {
            alerts.add(new Alert(Alert.Severity.WARN, message));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
        return location;
    }

    private static String toView(final ServletHttpRequest httpRequest, final String view) {
        return httpRequest.getBaseURI() + PathU.toPath("", view) + new Pather(httpRequest.getPathInfo()).getRight();
    }

    public final void applySession(final SubmitToken token, final XedRequest request) throws IOException {
        final String action = token.getAction();
        final String message = request.getBundle().getString("alert.action.not.implemented");
        if (App.Action.VALIDATE.equals(action)) {
            new SessionValidate(request.getSession(), request.getBundle(), alerts).validate();
        } else if (App.Action.PRETTY.equals(action)) {
            alerts.add(new Alert(Alert.Severity.WARN, message));
        } else if (App.Action.RELOAD.equals(action)) {
            new SessionReload(request.getState().getSessions(),
                    request.getSession(), request.getBundle(), alerts).reload();
        } else if (App.Action.SAVE.equals(action)) {
            alerts.add(new Alert(Alert.Severity.WARN, message));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void applyLocale(final NameTypeValues nameTypeValues) throws IOException {
        // apply to user state
        final Xed actionLocale = new XedActionLocale(null).update(nameTypeValues);
        final String localeID = actionLocale.getXPather().getText("/action:locale");
        locus = new LocusFactory().create(localeID, locus.getDateX());
        // apply to each xed session
        sessions.applyLocale(locus.getLocale());
    }
}
