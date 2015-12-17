package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.app.core.subsystem.ssh.SubsystemSSH;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Properties;

@SuppressWarnings({ "PMD.ExcessiveImports",
        "PMD.CyclomaticComplexity", "PMD.StdCyclomaticComplexity", "PMD.ModifiedCyclomaticComplexity" })
public class AppUserState {
    // lifetime
    private final Interval interval;
    // user identity token
    private final Principal principal;
    private final String submitID;
    // executor service
    private final UserExecutor userExecutor;
    // application state
    private final File userRoot;
    // configuration state
    private final XedUserState documentState;
    // table view states
    private final ViewStates viewStates;
    // local cache of remote resources
    private final ResourceCache cache;
    // user alerts
    private final Alerts alerts;
    // text filters (for file display)
    private final TextFilters textFilters;
    // widget subsystems
    private final SubsystemSSH ssh;
    // menu system
    private final MenuSystem menuSystem;

    // binary viewer state (hex rendering)
    private Page pageViewHex;


    public final Interval getInterval() {
        return interval;
    }

    public final Principal getPrincipal() {
        return principal;
    }

    public final String getSubmitID() {
        return submitID;
    }

    public final UserExecutor getUserExecutor() {
        return userExecutor;
    }

    public final File getUserRoot() {
        return userRoot;
    }

    public final XedUserState getDocumentState() {
        return documentState;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final ResourceCache getCache() {
        return cache;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public final TextFilters getTextFilters() {
        return textFilters;
    }

    public final SubsystemSSH getSSH() {
        return ssh;
    }

    public final MenuSystem getMenuSystem() {
        return menuSystem;
    }

    public final Page getPageViewHex() {
        return pageViewHex;
    }

    public final void setPageViewHex(final Page pageViewHex) {
        this.pageViewHex = pageViewHex;
    }


    public final Locus getLocus() {
        return documentState.getLocus();
    }

    public final Properties getProperties() {
        return documentState.getProperties();
    }

    public final String getCharset() {
        return documentState.getProperties().getProperty("charset", UTF8Codec.Const.UTF8);
    }

    public AppUserState(final Principal principal, final Date date, final File webappRoot,
                        final String submitID, final Locus locus) throws IOException {
        this.principal = principal;
        this.interval = new Interval(date, null);
        this.userRoot = AppFolder.getUserHome(webappRoot, principal);
        this.submitID = submitID;
        this.viewStates = new ViewStates();
        this.textFilters = new TextFilters();
        this.alerts = new Alerts();
        this.documentState = new XedUserState(webappRoot, principal, submitID, locus, alerts);
        this.userExecutor = new UserExecutor(principal, date, new File(SystemU.userHome()));
        this.ssh = new SubsystemSSH(alerts);
        this.menuSystem = new MenuSystem(submitID, new AppMenuFactory());
        this.cache = new ResourceCache();
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
                "view", "edit", "create", "viewGZ", "viewZIP", "viewTGZ", "viewHex");
        final Locus locus = documentState.getLocus();
        final Properties properties = documentState.getProperties();
        final Bundle bundle = new Bundle(new AppText(locus.getLocale()).getBundleCore());
        final String message = bundle.getString("alert.action.not.implemented");
        if (action == null) {
            getClass();
        } else if (App.Action.CLOSE.equals(action)) {
            doClose(token);
        } else if (App.Action.UPDATE_LOCALE.equals(action)) {
            documentState.applyLocale(httpArguments);
        } else if (App.Action.TEXT_FILTER.equals(action)) {
            new XedActionTextFilter(locus.getLocale()).updateTextFilters(textFilters, httpArguments);
        } else if ("clearCache".equals(action)) {
            doClearCache();
        } else if (App.Action.MENU.equals(action)) {
            menuSystem.toggle(object);
        } else if (App.Action.TOGGLE.equals(action)) {
            PropertiesU.toggleBoolean(properties, object);
        } else if (App.Action.MIME_TYPE.equals(action)) {
            PropertiesU.setProperty(properties, action, Value.defaultOnEmpty(object, null));
        } else if (App.Action.CHARSET.equals(action)) {
            getProperties().setProperty("charset", object);
        } else if (App.Action.HEX_VIEW_PARAM.equals(action)) {
            updateHexViewParam(object);
        } else if (views.contains(action)) {
            location = toView(httpRequest, action);
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
        return location;
    }

    private void doClose(final SubmitToken token) throws IOException {
        final String cacheName = token.getObject();
        final String resourceName = token.getObject2();
        if ("ssh".equals(cacheName)) {
            ssh.getCache().removeResource(resourceName);
        }
    }

    private void doClearCache() throws IOException {
        final long size = cache.getSize();
        cache.clear();
        final Bundle bundle = new Bundle(new AppText(getLocus().getLocale()).getBundleCore());
        alerts.add(new Alert(Alert.Severity.INFO, bundle.format("AppUserState.cache.clear", size)));
    }

    private void updateHexViewParam(final String object) {
        if ("first".equals(object)) {
            pageViewHex = Page.Factory.firstPage(pageViewHex);
        } else if ("prev".equals(object)) {
            pageViewHex = Page.Factory.prevPage(pageViewHex);
        } else if ("next".equals(object)) {
            pageViewHex = Page.Factory.nextPage(pageViewHex);
        } else if ("last".equals(object)) {
            pageViewHex = Page.Factory.lastPage(pageViewHex);
        } else if ("16".equals(object)) {
            pageViewHex.getProperties().remove(App.Action.HEX_VIEW_PARAM);
        } else if ("32".equals(object)) {
            pageViewHex.getProperties().setProperty(App.Action.HEX_VIEW_PARAM, object);
        } else if ("64".equals(object)) {
            pageViewHex.getProperties().setProperty(App.Action.HEX_VIEW_PARAM, object);
        }
    }

    private String toView(final ServletHttpRequest httpRequest, final String action) {
        final String pathInfoNewView = new Pather(httpRequest.getPathInfo()).getRight();
        return httpRequest.getBaseURI() + PathU.toPath("", action) + pathInfoNewView;
    }

    private static class Const {
        private static final int PAGE_HEX_VIEW = 4096;
    }
}
