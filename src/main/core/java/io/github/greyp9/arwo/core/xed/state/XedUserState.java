package io.github.greyp9.arwo.core.xed.state;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.locus.LocusFactory;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.util.PropertiesU;
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
import io.github.greyp9.arwo.core.xed.session.action.SessionCommit;
import io.github.greyp9.arwo.core.xed.session.action.SessionPretty;
import io.github.greyp9.arwo.core.xed.session.action.SessionReload;
import io.github.greyp9.arwo.core.xed.session.action.SessionRevision;
import io.github.greyp9.arwo.core.xed.session.action.SessionSave;
import io.github.greyp9.arwo.core.xed.session.action.SessionValidate;

import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Properties;
import java.util.TimeZone;

@SuppressWarnings("PMD.ExcessiveImports")
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

    public XedUserState(final File webappRoot, final Principal principal,
                        final String submitID, final Locus locus, final Alerts alerts) throws IOException {
        this.submitID = submitID;
        this.viewStates = new ViewStates();
        this.alerts = alerts;
        this.sessions = new XedSessionsFactory(webappRoot).getSessions(principal, locus);
        this.locus = locus;
        this.properties = new Properties();
        this.menuSystem = new MenuSystem(submitID, new XedMenuFactory());
        this.clipboard = new XedClipboard();
        applyLocale();
    }

    public final void applyGet(final NameTypeValues nameTypeValues) throws IOException {
        // from HTTP GET query string...
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            final String name = nameTypeValue.getName();
            final String value = nameTypeValue.getValueS();
            if (("toggle".equals(name)) && ("buttons".equals(value))) {
                PropertiesU.toggleBoolean(properties, value);
            }
        }
    }

    public final String applyPost(
            final SubmitToken token, final NameTypeValues httpArguments, final XedRequest request) throws IOException {
        // from HTTP POST form arguments...
        String location = request.getHttpRequest().getURI();
        final String action = token.getAction();
        final String object = token.getObject();
        final Collection<String> views = Arrays.asList(App.Action.UI, App.Action.XML, App.Action.XSD, App.Action.REV);
        final String message = request.getBundle().getString("alert.action.not.implemented");
        if (action == null) {
            getClass();
        } else if (App.Action.UPDATE_LOCALE.equals(action)) {
            applyLocale(httpArguments);
        } else if (App.Action.MENU.equals(action)) {
            menuSystem.toggle(object);
        } else if (App.Action.LOCALE.equals(action)) {
            PropertiesU.toggleBoolean(properties, action);
        } else if (App.Action.COMMIT.equals(action)) {
            PropertiesU.toggleBoolean(properties, action);
        } else if (views.contains(action)) {
            location = toView(request.getHttpRequest(), action);
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString(), null));
        }
        return location;
    }

    private static String toView(final ServletHttpRequest httpRequest, final String view) {
        return httpRequest.getBaseURI() + PathU.toPath("", view) + new Pather(httpRequest.getPathInfo()).getRight();
    }

    public final void applySession(
            final SubmitToken token, final NameTypeValues httpArguments, final XedRequest request) throws IOException {
        final String action = token.getAction();
        final String message = request.getBundle().getString("alert.action.not.implemented");
        final XedSessions sessionsApply = request.getState().getSessions();
        if (App.Action.VALIDATE.equals(action)) {
            new SessionValidate(request.getSession(), request.getBundle(), alerts).validate();
        } else if (App.Action.PRETTY.equals(action)) {
            new SessionPretty(sessionsApply, request.getSession(), request.getBundle(), alerts).pretty();
        } else if (App.Action.RELOAD.equals(action)) {
            new SessionReload(sessionsApply, request.getSession(), request.getBundle(), alerts).reload();
        } else if (App.Action.SAVE.equals(action)) {
            new SessionSave(request, request.getBundle(), alerts).save();
        } else if (App.Action.COMMIT.equals(action)) {
            final Properties propertiesApply = request.getState().getProperties();
            new SessionCommit(request.getSession(), request.getBundle(), alerts).commit(httpArguments, propertiesApply);
        } else if (App.Action.LOAD_REVISION.equals(action)) {
            new SessionRevision(sessionsApply, request.getSession(), request.getBundle(), alerts).loadRevision(token);
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString(), null));
        }
    }

    public final void applyLocale(final NameTypeValues nameTypeValues) throws IOException {
        // apply to user state
        final Xed actionLocale = new XedActionLocale(null).update(nameTypeValues);
        final String localeID = actionLocale.getXPather().getText("/action:locale");
        locus = new LocusFactory().create(localeID, locus.getDateX());
        // apply to each xed session
        sessions.applyLocale(locus.getLocale());
    }

    public final void applyLocale() throws IOException {
        final Preferences preferences = new Preferences(getSession("/app").getXed());
        final String tz = preferences.getTZ();
        final String dateFormat = preferences.getDateFormat();
        final String language = preferences.getLanguage();
        locus = new LocusFactory().create(language, new DateX(dateFormat, TimeZone.getTimeZone(tz)));
        // apply to each xed session
        sessions.applyLocale(locus.getLocale());
    }

    public final boolean isUnsavedState() throws IOException {
        boolean isUnsavedState = false;
        for (final XedSession session : sessions.getSessions()) {
            isUnsavedState |= (session.getDateModify() != null);
        }
        return isUnsavedState;
    }
}
