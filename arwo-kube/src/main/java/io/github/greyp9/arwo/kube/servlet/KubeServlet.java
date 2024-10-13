package io.github.greyp9.arwo.kube.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.submit.SubmitTokenU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.connection.KubeConnectionFactory;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.view.KubeEndpointView;
import io.github.greyp9.arwo.kube.view.KubePodListView;

import java.io.IOException;
import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class KubeServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 2646201175265843372L;

    private transient AppState appState;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Context context = AppNaming.lookupSubcontext(getServletContext().getContextPath());
        synchronized (this) {
            this.appState = (AppState) AppNaming.lookup(context, App.Naming.APP_STATE);
        }
    }

    @Override
    public final void destroy() {
        synchronized (this) {
            this.appState = null;
        }
    }

    @Override
    protected final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        final Pather patherResource = new Pather(httpRequest.getPathInfo());
        final String kubeResource = patherResource.getLeftToken();
        final Pather patherContext = new Pather(patherResource.getRight());
        final String kubeView = patherContext.getLeftToken();
        // process request
        AppUserState userState;
        synchronized (this) {
            userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        }

        final HttpResponse httpResponse;
        if (Value.isEmpty(kubeResource)) {
            httpResponse = new KubeEndpointView(httpRequest, userState, null).doGetResponse();
        } else {
            httpResponse = doGet(httpRequest, userState, kubeResource, kubeView);
        }

        // send response
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState userState;
        synchronized (this) {
            userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        }
        final HttpResponse httpResponse = doPost(httpRequest, userState);
        ServletU.write(httpResponse, response);
    }

    private HttpResponse doGet(final ServletHttpRequest httpRequest, final AppUserState userState,
                       final String kubeResource, final String kubeContext) throws IOException {
        final KubeConnectionFactory factory = new KubeConnectionFactory(httpRequest, userState);
        final ConnectionCache cache = userState.getKube().getCache();
        final KubeConnectionResource resource = (KubeConnectionResource) cache.getResource(kubeResource, factory);

        final HttpResponse httpResponse;
        if (resource == null) {
            httpResponse = HttpResponseU.to404();
        } else if ("pods".equals(kubeContext)) {
            httpResponse = new KubePodListView(httpRequest, userState, resource).doGetResponse();
        } else {
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }

    private HttpResponse doPost(final ServletHttpRequest httpRequest, final AppUserState userState) throws IOException {
        final byte[] entity = StreamU.read(httpRequest.getHttpRequest().getEntity());
        final NameTypeValues nameTypeValues = HttpArguments.toArguments(entity);
        final String submitID = userState.getSubmitID();
        HttpResponse httpResponse = HttpResponseU.to302(httpRequest.getURI());
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            if (submitID.equals(nameTypeValue.getName())) {
                final SubmitToken token = SubmitTokenU.fromString(nameTypeValue.getValueS());
                if (token != null) {
                    final String subject = token.getSubject();
                    if (App.Target.VIEW_STATE.equals(subject)) {
                        final Bundle bundle = new Bundle(new AppText(userState.getLocus().getLocale()).getBundleCore());
                        userState.getViewStates().apply(token, nameTypeValues, bundle, new Alerts());
                    }
                }
            }
        }
        return httpResponse;
    }
}
