package io.github.greyp9.arwo.app.ssh.core.view;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.ConnectionInfo;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;

public class SSHConnectionView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final SSHConnectionResource resource;
    private final Bundle bundle;

    public SSHConnectionView(final ServletHttpRequest httpRequest, final AppUserState userState,
                             final SSHConnectionResource resource, final Bundle bundle) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.resource = resource;
        this.bundle = bundle;
    }

    public final void addContent(final Element html) throws IOException {
        final String key = Value.join("/", App.Cache.SSH, App.Action.PROPERTIES);
        final boolean isPropertiesSSH = PropertiesU.isBoolean(userState.getProperties(), key);
        if (isPropertiesSSH) {
            httpRequest.getClass();
            final RowSetMetaData metaData = createMetaData("propertiesSSH");  // i18n metadata
            final RowSet rowSet = createRowSet(metaData);
            final Locus locus = userState.getLocus();
            final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
            final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
            TableU.addFooterStandard(table, bundle);
            final TableContext tableContext = new TableContext(
                    viewState, userState.getSubmitID(), App.CSS.TABLE, bundle, userState.getLocus());
            final TableView tableView = new TableView(table, tableContext);
            tableView.addContentTo(html);
        }
    }

    private RowSetMetaData createMetaData(final String tableID) {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData("name", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("value", Types.VARCHAR),  // i18n metadata
        };
        return new RowSetMetaData(tableID, columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final NameTypeValues properties = new NameTypeValues();
        addProperties(properties);
        final RowSet rowSet = new RowSet(metaData, null, null);
        for (final NameTypeValue property : properties) {
            final InsertRow insertRow = InsertRow.create(rowSet);
            final String nameLabel = bundle.getString(
                    Value.join(".", "sshConnectionType", property.getName()));  // i18n metadata
            insertRow.setNextColumn(nameLabel);
            insertRow.setNextColumn(property.getValue());
            rowSet.add(insertRow.getRow());
        }
        return rowSet;
    }

    private void addProperties(final NameTypeValues properties) throws IOException {
        if (resource != null) {
            properties.add(new NameTypeValue("name", resource.getName()));  // i18n metadata
            addProperties(properties, resource.getConnection());
        }
    }

    private void addProperties(final NameTypeValues properties, final SSHConnection sshConnection) throws IOException {
        if (sshConnection != null) {
            properties.add(new NameTypeValue("opened", sshConnection.getDateOpen()));  // i18n metadata
            properties.add(new NameTypeValue("last", sshConnection.getDateLast()));  // i18n metadata
            properties.add(new NameTypeValue("count", sshConnection.getCount()));  // i18n metadata
            properties.add(new NameTypeValue("millis", sshConnection.getMillis()));  // i18n metadata
            addProperties(properties, sshConnection.getConnection());
        }
    }

    private void addProperties(final NameTypeValues properties, final Connection connection) throws IOException {
        if (connection != null) {
            properties.add(new NameTypeValue("host", connection.getHostname()));
            properties.add(new NameTypeValue("port", connection.getPort()));
            final ConnectionInfo info = connection.getConnectionInfo();
            properties.add(new NameTypeValue("clientToServerCryptoAlgorithm", info.clientToServerCryptoAlgorithm));
            properties.add(new NameTypeValue("clientToServerMACAlgorithm", info.clientToServerMACAlgorithm));
            properties.add(new NameTypeValue("keyExchangeAlgorithm", info.clientToServerMACAlgorithm));
            properties.add(new NameTypeValue("keyExchangeCounter", info.clientToServerMACAlgorithm));
            properties.add(new NameTypeValue("serverToClientCryptoAlgorithm", info.serverToClientCryptoAlgorithm));
            properties.add(new NameTypeValue("serverToClientMACAlgorithm", info.serverToClientMACAlgorithm));
            final byte[] serverHostKey = info.serverHostKey;
            // ssh-keygen -E md5 -l -f /etc/ssh/ssh_host_rsa_key.pub
            // ssh-keygen -E sha256 -l -f /etc/ssh/ssh_host_rsa_key.pub
            properties.add(new NameTypeValue("MD5(key)", HexCodec.encode(HashU.md5(serverHostKey), ":")));
            properties.add(new NameTypeValue("SHA-1(key)", HexCodec.encode(HashU.sha1(serverHostKey), ":")));
            properties.add(new NameTypeValue("SHA-256(key)", HexCodec.encode(HashU.sha256(serverHostKey), ":")));
        }
    }
}
