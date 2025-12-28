package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.app.core.action.ActionCacheClear;
import io.github.greyp9.arwo.app.core.action.ActionRestart;
import io.github.greyp9.arwo.app.core.action.ActionStop;
import io.github.greyp9.arwo.app.core.subsystem.cifs.SubsystemCIFS;
import io.github.greyp9.arwo.app.core.subsystem.cron.SubsystemCron;
import io.github.greyp9.arwo.app.core.subsystem.dav.SubsystemWebDAV;
import io.github.greyp9.arwo.app.core.subsystem.interop.SubsystemInterop;
import io.github.greyp9.arwo.app.core.subsystem.jdbc.SubsystemJDBC;
import io.github.greyp9.arwo.app.core.subsystem.kube.SubsystemKube;
import io.github.greyp9.arwo.app.core.subsystem.local.SubsystemLocal;
import io.github.greyp9.arwo.app.core.subsystem.mail.SubsystemMail;
import io.github.greyp9.arwo.app.core.subsystem.s3.SubsystemS3;
import io.github.greyp9.arwo.app.core.subsystem.sh.SubsystemLSH;
import io.github.greyp9.arwo.app.core.subsystem.ssh.SubsystemSSH;
import io.github.greyp9.arwo.core.actiond.DeferredActions;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.cron.core.CronRunnable;
import io.github.greyp9.arwo.core.cron.service.CronService;
import io.github.greyp9.arwo.core.cron.service.CronServiceRegistrar;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.link.MetaLink;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.util.PropertiesX;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import io.github.greyp9.arwo.core.xed.action.XedActionRefresh;
import io.github.greyp9.arwo.core.xed.action.XedActionTextExpression;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.KeyStore;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

@SuppressWarnings({ "PMD.ExcessiveImports", "PMD.GodClass", "PMD.TooManyFields", "PMD.CouplingBetweenObjects",
        "PMD.CyclomaticComplexity", "PMD.StdCyclomaticComplexity", "PMD.ModifiedCyclomaticComplexity" })
public final class AppUserState {
    private final AppState appState;
    private final KeyStore keyStore;
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
    // private final ResourceCache cacheBlob;  // are two of these really needed?
    // user alerts
    private final Alerts alerts;
    // text filters (for file display)
    private final List<String> filtersRecent;
    private final Map<String, TextFilters> textFilters;
    // actions to be confirms
    private final DeferredActions deferredActions;
    // widget subsystems
    private final SubsystemCron cron;
    private final SubsystemLocal local;
    private final SubsystemLSH lsh;
    private final SubsystemKube kube;
    private final SubsystemS3 s3;
    private final SubsystemSSH ssh;
    private final SubsystemJDBC jdbc;
    private final SubsystemMail mail;
    private final SubsystemCIFS cifs;
    private final SubsystemInterop interop;
    private final SubsystemWebDAV webDAV;
    // menu system
    private final MenuSystem menuSystem;
    private final Properties menuSystemState;  // store for dynamic menu state

    // binary viewer state (hex rendering)
    private Page pageViewHex;

    // visualization view state
    private Page pageVisualization;

    public KeyStore getKeyStore() {
        return keyStore;
    }

    public Key getKey(final String key, final char[] password) throws IOException {
        try {
            return keyStore.getKey(key, password);
        } catch (GeneralSecurityException e) {
            throw new IOException(e);
        }
    }

    public CronService getCronService() {
        return appState.getCronService();
    }

    public Date getDateAppStart() {
        return appState.getDateStart();
    }

    public Date getDateSessionStart() {
        return interval.getDateStart();
    }

    public Interval getInterval() {
        return interval;
    }

    public Principal getPrincipal() {
        return principal;
    }

    public String getSubmitID() {
        return submitID;
    }

    public UserExecutor getUserExecutor() {
        return userExecutor;
    }

    public File getUserRoot() {
        return userRoot;
    }

    public XedUserState getDocumentState() {
        return documentState;
    }

    public ViewStates getViewStates() {
        return viewStates;
    }

    public ResourceCache getCache() {
        return cache;
    }

    /**
     * @deprecated convert usages to {@link #getCache()}; consolidation of two caches
     */
    public ResourceCache getCacheBlob() {
        return cache;
    }

    public Alerts getAlerts() {
        return alerts;
    }

    public List<String> getFiltersRecent() {
        return filtersRecent;
    }

    public TextFilters getTextFilters() {
        return getTextFilters("");
    }

    public TextFilters getTextFilters(final String context) {
        TextFilters textFiltersOp = textFilters.get(context);
        if (textFiltersOp == null) {
            textFiltersOp = new TextFilters();
            textFilters.put(context, textFiltersOp);
        }
        return textFiltersOp;
    }

    public DeferredActions getDeferredActions() {
        return deferredActions;
    }

    public SubsystemCron getCron() {
        return cron;
    }

    public SubsystemLocal getLocal() {
        return local;
    }

    public SubsystemLSH getLSH() {
        return lsh;
    }

    public SubsystemKube getKube() {
        return kube;
    }

    public SubsystemS3 getS3() {
        return s3;
    }

    public SubsystemSSH getSSH() {
        return ssh;
    }

    public SubsystemJDBC getJDBC() {
        return jdbc;
    }

    public SubsystemMail getMail() {
        return mail;
    }

    public SubsystemCIFS getCIFS() {
        return cifs;
    }

    public SubsystemInterop getInterop() {
        return interop;
    }

    public SubsystemWebDAV getWebDAV() {
        return webDAV;
    }

    public MenuSystem getMenuSystem() {
        return menuSystem;
    }

    public Properties getMenuSystemState() {
        return menuSystemState;
    }

    public Page getPageViewHex() {
        return pageViewHex;
    }

    public void setPageViewHex(final Page pageViewHex) {
        this.pageViewHex = pageViewHex;
    }

    public Page getPageVisualization() {
        return pageVisualization;
    }

    public Bundle getBundle() {
        return new Bundle(new AppText(getLocus().getLocale()).getBundleCore());
    }

    public Locus getLocus() {
        return documentState.getLocus();
    }

    public Locale getLocale() {
        return documentState.getLocus().getLocale();
    }

    public XedFactory getXedFactory() {
        return documentState.getFactory();
    }

    public ByteArrayInputStream getXHTML() {
        return documentState.getXHTML();
    }

    public Xed getConfig() throws IOException {
        return documentState.getConfig();
    }

    public Properties getProperties() {
        return documentState.getProperties();
    }

    public String getCharset() {
        return documentState.getProperties().getProperty(App.Action.CHARSET, UTF8Codec.Const.UTF8);
    }

    public AppUserState(final AppState appState, final Principal principal, final Date date, final File webappRoot,
                        final String submitID, final Locus locus) throws IOException {
        this.appState = appState;
        this.keyStore = (KeyStore) AppNaming.lookup(App.Secret.CONTEXT, App.Secret.NAME);
        this.principal = principal;
        this.interval = new Interval(date, null);
        this.userRoot = AppFolder.getUserHome(webappRoot, principal);
        this.submitID = submitID;
        this.filtersRecent = new ArrayList<>();
        this.textFilters = new TreeMap<>();
        //this.textFilters.put("", new TextFilters());
        this.alerts = new Alerts();
        this.documentState = new XedUserState(webappRoot, appState.getFactory(), principal, submitID, locus, alerts);
        this.documentState.getProperties().setProperty(App.Mode.VIEW_DOT, Boolean.TRUE.toString());
        this.viewStates = new ViewStates(new XedActionFilter(documentState.getFactory(), null), getConfig());
        this.userExecutor = new UserExecutor(principal, date, new File(SystemU.userHome()));
        this.deferredActions = new DeferredActions();
        this.cron = new SubsystemCron(documentState.getFactory());
        this.local = new SubsystemLocal(alerts, this.userRoot);
        this.lsh = new SubsystemLSH(alerts, this.userRoot);
        this.kube = new SubsystemKube(alerts);
        this.s3 = new SubsystemS3(alerts);
        this.ssh = new SubsystemSSH(alerts);
        this.jdbc = new SubsystemJDBC(alerts, this.userRoot);
        this.mail = new SubsystemMail(alerts);
        this.cifs = new SubsystemCIFS(alerts);
        this.interop = new SubsystemInterop(alerts);
        this.webDAV = new SubsystemWebDAV(alerts);
        this.menuSystem = new MenuSystem(submitID, new AppMenuFactory());
        this.menuSystemState = new Properties();
        this.cache = new ResourceCache(appState.getContextPath() + App.Servlet.CACHE);
        // this.cacheBlob = new ResourceCache(null); // needed?
        this.pageViewHex = Page.Factory.initPage(Const.PAGE_HEX_VIEW, new Properties());
        this.pageVisualization = Page.Factory.initPage(Const.PAGE_VISUALIZATION, new Properties());
    }

    public AppRequest getAppRequest(final ServletHttpRequest httpRequest) {
        return new AppRequest(httpRequest, submitID, getLocus(), getBundle(), alerts);
    }

    public ResultsContext getResultsContext(final ServletHttpRequest httpRequest) throws IOException {
        final XedActionFilter filter = new XedActionFilter(getXedFactory(), getLocale());
        final MetaLink metaLink = getMetaLink(httpRequest);
        return new ResultsContext(viewStates, filter, getLocus(), getBundle(), alerts, submitID, metaLink);
    }

    public MetaLink getMetaLink(final ServletHttpRequest httpRequest) {
        MetaLink metaLink = new MetaLink(null, null);
        final String persist = httpRequest.getHttpRequest().getHeader(App.Header.RESULT);
        if (persist != null) {
            final String linkRoot = PathU.toPath(httpRequest.getContextPath(), App.Servlet.LFS, App.Mode.VIEW_R);
            final String servletPath = httpRequest.getServletPath();
            final String pathInfo = httpRequest.getPathInfo();
            final String filenameBase = Value.defaultOnEmpty(new FileX(pathInfo).getFilename(), null);
            final String filename = Value.join(Http.Token.HYPHEN, persist, filenameBase);
            final String offset = PathU.toPath("", CronRunnable.Const.RESULT, servletPath, filename);
            metaLink = new MetaLink(userRoot, linkRoot, offset);
        }
        return metaLink;
    }

    public String applyPost(final SubmitToken token, final NameTypeValues httpArguments,
                                  final ServletHttpRequest httpRequest) throws IOException {
        return applyPost(token, httpArguments, httpRequest, "");
    }

    public String applyPost(final SubmitToken token, final NameTypeValues httpArguments,
                                  final ServletHttpRequest httpRequest, final String context) throws IOException {
        httpArguments.getClass();
        // from HTTP POST form arguments...
        String location = httpRequest.getURI();
        final String action = token.getAction();
        final String object = token.getObject();
        final String object2 = token.getObject2();
        final Collection<String> views = Arrays.asList(App.Mode.CREATE_F, App.Mode.CREATE_D, App.Mode.EDIT,
                App.Mode.DELETE, App.Mode.FIND, App.Mode.VIEW, App.Mode.VIEW_HEAD, App.Mode.VIEW_TAIL,
                App.Mode.VIEW_GZ, App.Mode.VIEW_ZIP, App.Mode.VIEW_TGZ, App.Mode.VIEW_HEX, App.Mode.VIEW_R);
        final Properties properties = documentState.getProperties();
        final Bundle bundle = getBundle();
        final String message = bundle.getString("alert.action.not.implemented");
        if (action == null) {
            getClass();
        } else if (App.Action.COMMAND.equals(action)) {
            location = doFavoriteSH(httpRequest, location, object);
        } else if (App.Action.RESET.equals(action)) {
            location = doLogOff(httpRequest);
        } else if (App.Action.STOP.equals(action)) {
            location = doStop(httpRequest);
        } else if (App.Action.RESTART.equals(action)) {
            location = doRestart(httpRequest);
        } else if (App.Action.CLOSE.equals(action)) {
            doClose(token);
        } else if (App.Action.UPDATE_LOCALE.equals(action)) {
            documentState.applyLocale(httpArguments);
        } else if (App.Action.TEXT_EXPRESSION.equals(action)) {
            final TextFilters textFiltersOp = getTextFilters(context);
            new XedActionTextExpression(getXedFactory(), getLocale(), textFiltersOp)
                    .updateTextFilters(filtersRecent, httpArguments);
        } else if (App.Action.TEXT_FILTER.equals(action)) {
            final TextFilters textFiltersOp = getTextFilters(context);
            new XedActionTextFilter(getXedFactory(), getLocale()).updateTextFilters(textFiltersOp, httpArguments);
        } else if (App.Action.CLEAR.equals(action)) {
            doClearCache();
        } else if (App.Action.REFRESH.equals(action) && (App.Object.TABLE.equals(object))) {
            cache.putRowSet(httpRequest.getURI(), null);
        } else if (App.Action.REFRESH.equals(action)) {
            PropertiesU.toggleBoolean(properties, action);
        } else if (App.Action.RELOAD.equals(action)) {
            location = object;
        } else if (App.Action.MENU2.equals(action)) {
            final boolean stateFrom = PropertiesU.isBoolean(menuSystemState, object);
            PropertiesU.clearStartsWith(menuSystemState, Pather.getParent(object));
            PropertiesU.setBoolean(menuSystemState, object, !stateFrom);  // store for dynamic menus
        } else if (App.Action.MENU.equals(action)) {
            menuSystem.toggle(object);  // store for fixed menus
            final boolean stateFrom = PropertiesU.isBoolean(menuSystemState, object);
            final String parent = object.replace(object2, "");
            PropertiesU.clearStartsWith(menuSystemState, parent);
            PropertiesU.setBoolean(menuSystemState, object, !stateFrom);  // store for dynamic menus
            if (!Value.isEmpty(object2)) {
                location = PathU.toDir(httpRequest.getBaseURI(), object2);
            }
        } else if (App.Action.TOGGLE.equals(action)) {
            PropertiesU.toggleBoolean(properties, object);
        } else if (App.Action.MIME_TYPE.equals(action)) {
            PropertiesU.setProperty(properties, action, Value.defaultOnEmpty(object, null));
        } else if (App.Action.CHARSET.equals(action)) {
            getProperties().setProperty(App.Action.CHARSET, object);
        } else if (App.Action.UPDATE.equals(action) && App.Action.REFRESH.equals(object)) {
            final String intervalOp = new XedActionRefresh(getXedFactory(), getLocale()).getInterval(httpArguments);
            getProperties().setProperty(XedActionRefresh.Const.KEY, intervalOp);
        } else if (App.Action.HEX_VIEW_PARAM.equals(action)) {
            updateHexViewParam(object);
        } else if (App.Action.NAV_PARAM.equals(action)) {
            updateNavParam(object, object2);
        } else if (views.contains(action)) {
            location = toView(httpRequest, action);
        } else if (App.Action.CRON_OFF.equals(action)) {
            doCronOff(httpRequest);
        } else if (App.Action.CRON_ON.equals(action)) {
            doCronOn(httpRequest);
        } else if (App.Action.CRON_NOW.equals(action)) {
            doCronNow(httpRequest, token);
        } else if (App.Action.ALERT.equals(action)) {
            alerts.remove(object2);
            deferredActions.apply(object, object2, bundle, alerts);
        } else if (App.Action.NAV_PARENT.equals(action)) {
            location = httpRequest.getURI() + "..";
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
        return location;
    }

    public Collection<ConnectionCache> getConnectionCaches() {
        return Arrays.asList(
                kube.getCache(),
                ssh.getCache(),
                jdbc.getCache(),
                mail.getCacheSMTP(),
                mail.getCacheIMAP(),
                mail.getCachePOP3(),
                cifs.getCache(),
                interop.getCache(),
                webDAV.getCache());
    }

    public void close(final Date date) throws IOException {
        doClearCache();
        interval.setDateFinish(date);
        final Collection<ConnectionCache> connectionCaches = getConnectionCaches();
        for (final ConnectionCache cacheIt : connectionCaches) {
            while (!cacheIt.getResources().isEmpty()) {
                final String name = cacheIt.getResources().iterator().next().getName();
                cacheIt.removeResource(name);
            }
        }
        userExecutor.stop(date);
    }

    private boolean isUserState() throws IOException {
        final boolean isSSH = (!ssh.getCache().getResources().isEmpty());
        final boolean isWebDAV = (!webDAV.getCache().getResources().isEmpty());
        final boolean isDocument = documentState.isUnsavedState();
        return (isSSH || isWebDAV || isDocument);
    }

    private String doFavoriteSH(final ServletHttpRequest httpRequest, final String location, final String object)
            throws IOException {
        String locationUpdate = location;
        final Xed xed = documentState.getSession(App.Servlet.FAVORITES).getXed();
        final XedNav nav = new XedNav(xed);
        final String xpath = String.format("/app:favorites/app:lshFavorites/app:lshFavorite[@name='%s']", object);
        final XedCursor cursor = nav.findX(xpath);
        if (cursor != null) {
            final String context = cursor.getValue(cursor.getChildInstance(App.Settings.CONTEXT));
            final String command = cursor.getValue(cursor.getChildInstance(App.Settings.COMMAND));
            getProperties().setProperty(App.Settings.COMMAND, command);
            locationUpdate = PathU.toDir(httpRequest.getBaseURI(), context);
        }
        return locationUpdate;
    }

    private String doLogOff(final ServletHttpRequest httpRequest) throws IOException {
        String location = Http.Token.SLASH;  // root URL for service
        if (isUserState()) {
            alerts.add(new Alert(Alert.Severity.INFO, getBundle().getString("AppUserState.unsaved.state")));
            location = httpRequest.getContextPath() + App.Servlet.DASH;
        } else {
            appState.removeUserState(principal, httpRequest.getDate());
        }
        return location;
    }

    private String doStop(final ServletHttpRequest httpRequest) throws IOException {
        String location = httpRequest.getURI();
        if (isUserState()) {
            alerts.add(new Alert(Alert.Severity.INFO, getBundle().getString("AppUserState.unsaved.state")));
            location = httpRequest.getContextPath() + App.Servlet.DASH;
        } else {
            final ActionStop action = new ActionStop();
            deferredActions.add(action);
            final String message = getBundle().format("AppUserState.stop.message");
            alerts.add(new Alert(Alert.Severity.QUESTION, message, null, null, action.getActions()));
        }
        return location;
    }

    private String doRestart(final ServletHttpRequest httpRequest) throws IOException {
        String location = httpRequest.getURI();
        if (isUserState()) {
            alerts.add(new Alert(Alert.Severity.INFO, getBundle().getString("AppUserState.unsaved.state")));
            location = httpRequest.getContextPath() + App.Servlet.DASH;
        } else {
            final ActionRestart action = new ActionRestart();
            deferredActions.add(action);
            final String message = getBundle().format("AppUserState.restart.message");
            alerts.add(new Alert(Alert.Severity.QUESTION, message, null, null, action.getActions()));
        }
        return location;
    }

    private void doClose(final SubmitToken token) throws IOException {
        final String cacheName = token.getObject();
        final String resourceName = token.getObject2();
        if (App.Cache.CIFS.equals(cacheName)) {
            cifs.getCache().removeResource(resourceName);
        } else if (App.Cache.JDBC.equals(cacheName)) {
            jdbc.getCache().removeResource(resourceName);
        } else if (App.Cache.KUBE.equals(cacheName)) {
            kube.getCache().removeResource(resourceName);
        } else if (App.Cache.S3.equals(cacheName)) {
            s3.getCache().removeResource(resourceName);
        } else if (App.Cache.SSH.equals(cacheName)) {
            ssh.getCache().removeResource(resourceName);
        } else if (App.Cache.DAV.equals(cacheName)) {
            webDAV.getCache().removeResource(resourceName);
        } else if (App.Cache.WSH.equals(cacheName)) {
            interop.getCache().removeResource(resourceName);
        } else if (App.Cache.SMTP.equals(cacheName)) {
            mail.getCacheSMTP().removeResource(resourceName);
        } else if (App.Cache.IMAP.equals(cacheName)) {
            mail.getCacheIMAP().removeResource(resourceName);
        } else if (App.Cache.POP3.equals(cacheName)) {
            mail.getCachePOP3().removeResource(resourceName);
        }
    }

    private void doClearCache() throws IOException {
        final ActionCacheClear action = new ActionCacheClear(cache);  // (cache, cacheBlob)
        deferredActions.add(action);
        final String message = getBundle().format("AppUserState.cache.clear.message");
        alerts.add(new Alert(Alert.Severity.QUESTION, message, null, null, action.getActions()));
    }

    private void updateHexViewParam(final String object) {
        if (ViewState.Nav.FIRST.equals(object)) {
            pageViewHex = Page.Factory.firstPage(pageViewHex);
        } else if (ViewState.Nav.PREVIOUS.equals(object)) {
            pageViewHex = Page.Factory.prevPage(pageViewHex);
        } else if (ViewState.Nav.NEXT.equals(object)) {
            pageViewHex = Page.Factory.nextPage(pageViewHex);
        } else if (ViewState.Nav.LAST.equals(object)) {
            pageViewHex = Page.Factory.lastPage(pageViewHex);
        } else if (App.Hex.WIDTH_16.equals(object)) {
            pageViewHex.getProperties().remove(App.Action.HEX_VIEW_PARAM);
        } else if (App.Hex.WIDTH_32.equals(object)) {
            pageViewHex.getProperties().setProperty(App.Action.HEX_VIEW_PARAM, object);
        } else if (App.Hex.WIDTH_64.equals(object)) {
            pageViewHex.getProperties().setProperty(App.Action.HEX_VIEW_PARAM, object);
        }
    }

    private void updateNavParam(final String object, final String object2) {
        final PropertiesX properties = new PropertiesX(pageVisualization.getProperties());
        if (ViewState.Nav.FIRST.equals(object)) {
            properties.setLong(object2, 0L);
            pageVisualization = Page.Factory.firstPage(pageVisualization);
        } else if (ViewState.Nav.PREVIOUS.equals(object)) {
            properties.addLong(object2, -1L);
            pageVisualization = Page.Factory.prevPage(pageVisualization);
        } else if (ViewState.Nav.NEXT.equals(object)) {
            properties.addLong(object2, 1L);
            pageVisualization = Page.Factory.nextPage(pageVisualization);
        } else if (ViewState.Nav.LAST.equals(object)) {
            properties.setLong(object2, Long.MAX_VALUE);
            pageVisualization = Page.Factory.lastPage(pageVisualization);
        }
    }

    private String toView(final ServletHttpRequest httpRequest, final String action) {
        final Pather patherPathInfo = new Pather(httpRequest.getPathInfo());
        final String pathInfoNewView = patherPathInfo.getRight();
        return httpRequest.getBaseURI() + PathU.toPath("", action) + pathInfoNewView;
    }

    private void doCronOff(final ServletHttpRequest httpRequest) throws IOException {
        final Date date = httpRequest.getDate();
        final String authorization = httpRequest.getHttpRequest().getHeader(Http.Header.AUTHORIZATION);
        final CronService cronService = appState.getCronService();
        final RowSet rowSet = cron.getRowSetCron();
        new CronServiceRegistrar(date, authorization, principal, getBundle(), alerts, cronService, rowSet).
                unregister();
    }

    private void doCronOn(final ServletHttpRequest httpRequest) throws IOException {
        doCronOff(httpRequest);
        final Date date = httpRequest.getDate();
        final String authorization = httpRequest.getHttpRequest().getHeader(Http.Header.AUTHORIZATION);
        final CronService cronService = appState.getCronService();
        final RowSet rowSet = cron.getRowSetCron();
        new CronServiceRegistrar(date, authorization, principal, getBundle(), alerts, cronService, rowSet).
                register(documentState.getSession(App.Servlet.SETTINGS).getXed());
    }

    private void doCronNow(final ServletHttpRequest httpRequest, final SubmitToken token) throws IOException {
        appState.getCronService().runNow(principal, httpRequest.getDate(), token.getObject(), token.getObject2());
    }

    private static class Const {
        private static final int PAGE_HEX_VIEW = 4096;
        private static final int PAGE_VISUALIZATION = 1;
    }
}
