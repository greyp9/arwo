package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.text.TextFilters;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Properties;

public class AppUserState {
    private final Principal principal;
    private final Interval interval;
    private final File userHome;
    private final String submitID;
    private final ViewStates viewStates;
    private final TextFilters textFilters;
    private final Locus locus;
    private final Alerts alerts;
    private final XedUserState documentState;
    // menu system
    private final MenuSystem menuSystem;
    // resource caches (preloaded stuff)
    private final ResourceCache cache;
    // connection entries
    private final ConnectionCache cacheSSH;
    // properties
    private final Properties properties;
    // binary viewer state (hex rendering)
    private Page pageViewHex;

    public final Principal getPrincipal() {
        return principal;
    }

    public final Interval getInterval() {
        return interval;
    }

    public final File getUserHome() {
        return userHome;
    }

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final TextFilters getTextFilters() {
        return textFilters;
    }

    public final Locus getLocus() {
        return locus;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public final XedUserState getDocumentState() {
        return documentState;
    }

    public final MenuSystem getMenuSystem() {
        return menuSystem;
    }

    public final ResourceCache getCache() {
        return cache;
    }

    public final ConnectionCache getCacheSSH() {
        return cacheSSH;
    }

    public final Properties getProperties() {
        return properties;
    }

    public final Page getPageViewHex() {
        return pageViewHex;
    }

    public final void setPageViewHex(final Page pageViewHex) {
        this.pageViewHex = pageViewHex;
    }

    public AppUserState(final Principal principal, final Date date, final File webappRoot,
                        final String submitID, final Locus locus) throws IOException {
        this.principal = principal;
        this.interval = new Interval(date, null);
        this.userHome = AppFolder.getUserHome(webappRoot, principal);
        this.submitID = submitID;
        this.viewStates = new ViewStates();
        this.textFilters = new TextFilters();
        this.locus = locus;
        this.alerts = new Alerts();
        this.documentState = new XedUserState(webappRoot, principal, submitID, locus, alerts);
        this.menuSystem = new MenuSystem(submitID, new AppMenuFactory());
        this.cache = new ResourceCache();
        this.cacheSSH = new ConnectionCache("ssh", alerts);
        this.properties = new Properties();
        this.pageViewHex = Page.Factory.initPage(Const.PAGE_HEX_VIEW, new Properties());
    }

    public final String applyPost(final SubmitToken token, final NameTypeValues httpArguments,
                                  final ServletHttpRequest httpRequest) throws IOException {
        httpArguments.getClass();
        // from HTTP POST form arguments...
        String location = httpRequest.getURI();
        final String action = token.getAction();
        final String object = token.getObject();
        final Collection<String> views = Arrays.asList(
                "view", "view16", "edit", "edit16",
                "viewGZ", "viewZIP", "viewTGZ", "viewHex");
        final Bundle bundle = new Bundle(new AppText(locus.getLocale()).getBundleCore());
        final String message = bundle.getString("alert.action.not.implemented");
        if (action == null) {
            getClass();
        } else if (App.Action.MENU.equals(action)) {
            menuSystem.toggle(object);
        } else if (App.Action.TOGGLE.equals(action)) {
            PropertiesU.toggleBoolean(properties, object);
        } else if (views.contains(action)) {
            location = switchView(httpRequest, action);
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
        return location;
    }

    private String switchView(final ServletHttpRequest httpRequest, final String action) {
        final String pathInfoNewView = new Pather(httpRequest.getPathInfo()).getRight();
        return httpRequest.getBaseURI() + PathU.toPath("", action) + pathInfoNewView;
    }

    private static class Const {
        private static final int PAGE_HEX_VIEW = 4096;
    }
}
