package io.github.greyp9.arwo.app.jdbc.sh.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.jdbc.connection.JDBCConnectionFactory;
import io.github.greyp9.arwo.app.jdbc.connection.JDBCConnectionResource;
import io.github.greyp9.arwo.app.jdbc.sh.core.JDBCRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.jdbc.connection.JDBCConnection;
import io.github.greyp9.arwo.core.jdbc.query.Query;
import io.github.greyp9.arwo.core.jdbc.runnable.QueryContext;
import io.github.greyp9.arwo.core.jdbc.runnable.QueryRunnable;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.core.xed.action.XedActionSQL;

import java.io.IOException;

public class SHQueueCommand {
    private final JDBCRequest request;

    public SHQueueCommand(final JDBCRequest request) {
        this.request = request;
    }

    public final String doAction(final String locationIn, final NameTypeValues httpArguments) throws IOException {
        String location = locationIn;
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        final Bundle bundle = request.getBundle();
        final Alerts alerts = request.getAlerts();
        final String server = request.getServer();
        final JDBCConnectionFactory factory = new JDBCConnectionFactory(httpRequest, userState, bundle, alerts);
        final ConnectionCache cache = userState.getJDBC().getCache();
        final JDBCConnectionResource resource = (JDBCConnectionResource) cache.getResource(server, factory);
        if (resource == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SFTPHandlerPostMultipart.no.connect", server)));
        } else {
            location = doAction(httpArguments, resource.getConnection());
        }
        return location;
    }

    private String doAction(final NameTypeValues httpArguments, final JDBCConnection connection) throws IOException {
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        final String server = request.getServer();
        // queue command text for execution
        final String sql = new XedActionSQL(userState.getXedFactory()).getSQL(httpArguments);
        final Query query = new Query(server, httpRequest.getDate().getTime(), sql);
        userState.getJDBC().getHistory().add(query);
        userState.getJDBC().getProperties().setProperty(App.Settings.SQL, sql);
        // runnable to execute commands
        final UserExecutor userExecutor = userState.getUserExecutor();
        final ResourceCache cacheBlob = userState.getCacheBlob();
        // set call to persist results
        final String filename = String.format("%s.results", DateX.toFilename(httpRequest.getDate()));
        httpRequest.getHttpRequest().getHeaders().add(new NameTypeValue(App.Header.RESULT, filename));
        // calculate file to which results will be stored, link for "complete" message
        final ResultsContext resultsContext = userState.getResultsContext(httpRequest);
        final QueryContext context = new QueryContext(connection, cacheBlob, resultsContext);
        final QueryRunnable runnable = new QueryRunnable(query, context);
        userExecutor.getRunnables().add(runnable);
        userExecutor.getExecutorCommand().execute(runnable);
        // redirect to resource for execution monitor
        final DateX dateX = DateX.Factory.createURL();
        final String commandID = dateX.toString(httpRequest.getDate());
        return PathU.toDir(httpRequest.getBaseURI(), server, commandID);
    }
}
