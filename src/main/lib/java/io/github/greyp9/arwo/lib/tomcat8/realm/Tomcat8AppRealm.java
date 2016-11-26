package io.github.greyp9.arwo.lib.tomcat8.realm;

import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.security.realm.AppRealm;
import io.github.greyp9.arwo.core.security.realm.AppRealmContainer;
import io.github.greyp9.arwo.core.security.realm.AuthPrincipal;
import org.apache.catalina.Container;
import org.apache.catalina.Context;
import org.apache.catalina.CredentialHandler;
import org.apache.catalina.Wrapper;
import org.apache.catalina.connector.Request;
import org.apache.catalina.connector.Response;
import org.apache.catalina.core.StandardContext;
import org.apache.tomcat.util.descriptor.web.SecurityConstraint;
import org.ietf.jgss.GSSContext;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.security.Principal;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.logging.Logger;

@SuppressWarnings("PMD.TooManyMethods")
public class Tomcat8AppRealm implements org.apache.catalina.Realm, AppRealmContainer {
    private AppRealm appRealm = new AppRealm("", "", new ArrayList<AuthPrincipal>());
    private StandardContext standardContext;

    @Override
    public final Container getContainer() {
        return standardContext;
    }

    @Override
    public final void setContainer(final Container container) {
        this.standardContext = ((container instanceof StandardContext) ? ((StandardContext) container) : null);
    }

    @Override
    public final void setAppRealm(final AppRealm appRealm) {
        if (appRealm == null) {
            this.appRealm.clear();
            this.appRealm = null;
        } else {
            this.appRealm = appRealm;
            this.standardContext.setRealm(this);
            this.standardContext.getLoginConfig().setRealmName(appRealm.getName());
        }
    }

    @Override
    public final CredentialHandler getCredentialHandler() {
        return null;
    }

    @Override
    public final void setCredentialHandler(final CredentialHandler handler) {
        // purposefully empty
    }

    @Override
    public final void addPropertyChangeListener(final PropertyChangeListener listener) {
        // purposefully empty
    }

    @Override
    public final Principal authenticate(final String username, final String credentials) {
        return ((appRealm == null) ? null : appRealm.authenticate(username, credentials));
    }

    @Override
    public final Principal authenticate(final String username) {
        return null;
    }

    @SuppressWarnings({ "PMD.UseObjectForClearerAPI", "checkstyle:parameternumber" })
    @Override
    public final Principal authenticate(
            final String username, final String digest, final String nonce, final String nc,
            final String cnonce, final String qop, final String realm, final String md5a2) {
        return null;
    }

    @Override
    public final Principal authenticate(final GSSContext gssContext, final boolean storeCreds) {
        return null;
    }

    @SuppressWarnings("PMD.UseVarargs")
    @Override
    public final Principal authenticate(final X509Certificate[] certs) {
        return appRealm.authenticate(null, certs);
    }

    @Override
    public final void backgroundProcess() {
        final Logger logger = Logger.getLogger(getClass().getName());
        final String contextPath = standardContext.getEncodedPath();
        final javax.naming.Context context = AppNaming.lookupSubcontext(contextPath);
        final Object o = AppNaming.lookup(context, AppRealmContainer.NAMING_CONTAINER);
        if (o == null) {
            logger.info(String.format("bind(%s, %s@%s, %s@%s)", contextPath,
                    context.getClass().getSimpleName(), Integer.toHexString(context.hashCode()),
                    this.getClass().getSimpleName(), Integer.toHexString(this.hashCode())));
            AppNaming.bind(context, AppRealmContainer.NAMING_CONTAINER, this);
        }
    }

    @Override
    public final SecurityConstraint[] findSecurityConstraints(final Request request, final Context context) {
        return context.findConstraints();
    }

    @Override
    public final boolean hasResourcePermission(
            final Request request, final Response response,
            final SecurityConstraint[] constraints, final Context context) throws IOException {
        boolean hasPermission = true;
        final Principal principal = request.getPrincipal();
        for (final SecurityConstraint constraint : constraints) {
            if (!satisfiesConstraint(principal, constraint)) {
                hasPermission = false;
                break;
            }
        }
        if (!hasPermission) {
            response.sendError(HttpURLConnection.HTTP_FORBIDDEN);
        }
        return hasPermission;
    }

    private boolean satisfiesConstraint(final Principal principal, final SecurityConstraint constraint) {
        boolean satisfies = false;
        final String[] roles = constraint.findAuthRoles();
        if (roles.length == 0) {
            satisfies = true;
        } else {
            for (final String role : roles) {
                if (hasRole(null, principal, role)) {
                    satisfies = true;
                    break;
                }
            }
        }
        return satisfies;
    }

    @Override
    public final boolean hasRole(final Wrapper wrapper, final Principal principal, final String role) {
        return appRealm.isUserInRole(principal, role);
    }

    @SuppressWarnings("PMD.UseVarargs")
    @Override
    public final boolean hasUserDataPermission(
            final Request request, final Response response,
            final SecurityConstraint[] constraints) throws IOException {
        return true;
    }

    @Override
    public final void removePropertyChangeListener(final PropertyChangeListener listener) {
        // purposefully empty
    }
}
