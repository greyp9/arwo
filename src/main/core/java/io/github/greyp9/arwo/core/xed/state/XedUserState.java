package io.github.greyp9.arwo.core.xed.state;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.locus.LocusFactory;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.action.XedActionCommit;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.menu.XedMenuFactory;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessions;
import io.github.greyp9.arwo.core.xed.session.XedSessionsFactory;
import io.github.greyp9.arwo.core.xed.session.action.SessionCommit;
import io.github.greyp9.arwo.core.xed.session.action.SessionPretty;
import io.github.greyp9.arwo.core.xed.session.action.SessionReload;
import io.github.greyp9.arwo.core.xed.session.action.SessionRevision;
import io.github.greyp9.arwo.core.xed.session.action.SessionSave;
import io.github.greyp9.arwo.core.xed.session.action.SessionValidate;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Locale;
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
    // configuration state
    private final XedFactory factory;
    // document content clipboard
    private final XedClipboard clipboard;
    // template xhtml
    private final byte[] xhtml;
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
        return sessions.getSession(contextPath);
    }

    public final XedSession getSession(final String contextPath, final XedEntry xedEntry) throws IOException {
        return sessions.getSession(contextPath, xedEntry);
    }

    public final Properties getProperties() {
        return properties;
    }

    public final MenuSystem getMenuSystem() {
        return menuSystem;
    }

    public final XedFactory getFactory() {
        return factory;
    }

    public final XedClipboard getClipboard() {
        return clipboard;
    }

    public final ByteArrayInputStream getXHTML() {
        return new ByteArrayInputStream(xhtml);
    }

    public final Locus getLocus() {
        return locus;
    }

    public final Locale getLocale() {
        return locus.getLocale();
    }

    public XedUserState(final File webappRoot, final XedFactory factory, final Principal principal,
                        final String submitID, final Locus locus, final Alerts alerts) throws IOException {
        this.submitID = submitID;
        this.factory = new XedFactory(factory);
        final XedActionFilter filter = new XedActionFilter(factory, null);
        this.viewStates = new ViewStates(filter);
        this.alerts = alerts;
        this.sessions = new XedSessionsFactory(webappRoot, factory).getSessions(principal);
        this.locus = locus;
        this.properties = new Properties();
        this.menuSystem = new MenuSystem(submitID, new XedMenuFactory());
        this.clipboard = new XedClipboard();
        this.xhtml = StreamU.read(ResourceU.resolve(App.Html.UI));
        applyLocale();
    }

    public final void applyGet(final NameTypeValues nameTypeValues) throws IOException {
        // from HTTP GET query string...
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            final String name = nameTypeValue.getName();
            final String value = nameTypeValue.getValueS();
            if ((App.Action.TOGGLE.equals(name)) && (App.CSS.BUTTONS.equals(value))) {
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
        final Collection<String> views = Arrays.asList(
                App.Action.UI, App.Action.XML, App.Action.XSD, App.Action.REVISIONS);
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
        } else if (App.Action.REVEAL.equals(action)) {
            PropertiesU.toggleBoolean(properties, action);
        } else if (views.contains(action)) {
            location = toView(request.getHttpRequest(), action);
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
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
            final String comment = new XedActionCommit(factory, getLocale()).getComment(httpArguments);
            new SessionCommit(request.getSession(), request.getBundle(), alerts).commit(comment, propertiesApply);
        } else if (App.Action.LOAD_REVISION.equals(action)) {
            new SessionRevision(sessionsApply, request.getSession(), request.getBundle(), alerts).loadRevision(token);
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
    }

    public final void applyLocale(final NameTypeValues nameTypeValues) throws IOException {
        // apply to user state
        final Xed actionLocale = new XedActionLocale(factory, null).update(nameTypeValues);
        final String localeID = actionLocale.getXPather().getText("/action:locale");  // i18n xpath
        locus = new LocusFactory().create(localeID, locus.getDateX());
    }

    public final void applyLocale() throws IOException {
        final Preferences preferences = new Preferences(getSession(App.Servlet.SETTINGS).getXed());
        final String tz = preferences.getTZ();
        final String dateFormat = preferences.getDateFormat();
        final String language = preferences.getLanguage();
        locus = new LocusFactory().create(language, new DateX(dateFormat, TimeZone.getTimeZone(tz)));
    }

    public final boolean isUnsavedState() throws IOException {
        boolean isUnsavedState = false;
        for (final XedSession session : sessions.getSessions()) {
            isUnsavedState |= (session.getDateModify() != null);
        }
        return isUnsavedState;
    }
}
