package io.github.greyp9.arwo.app.core.subsystem.jdbc;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.jdbc.query.History;
import io.github.greyp9.arwo.core.jdbc.query.Query;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.xml.ResultsReader;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;

public class SubsystemJDBC {
    // connection entries
    private final ConnectionCache cache;
    // usage history
    private final History history;
    // properties
    private final Properties properties;

    public final ConnectionCache getCache() {
        return cache;
    }

    public final History getHistory() {
        return history;
    }

    public final Properties getProperties() {
        return properties;
    }

    public SubsystemJDBC(final Alerts alerts, final File folder) {
        this.cache = new ConnectionCache(App.Cache.JDBC, alerts);
        this.history = new History();
        loadRecent(alerts, folder);
        this.properties = new Properties();
    }

    private void loadRecent(final Alerts alerts, final File folder) {
        final File folderHistory = new File(folder, "result/jdbc");
        final Collection<File> files = new FindInFolderQuery(folderHistory, "*.results", false).getFound();
        final int recentCount = 10;
        final long skip = Math.max(0, (files.size() - recentCount));
        final List<File> filesRecent = files.stream().sorted().skip(skip).collect(Collectors.toList());
        final ResultsReader reader = new ResultsReader();
        for (final File file : filesRecent) {
            try {
                final Results results = reader.readFrom(
                        new MetaFile(null, null, new ByteArrayInputStream(StreamU.read(file))));
                final Date dateStart = results.getInterval().getDateStart();
                final Query query = new Query(results.getContext(), dateStart.getTime(),
                        history.getNewID(dateStart), results.getCommand(), results);
                history.add(query);
            } catch (IOException e) {
                alerts.add(new Alert(Alert.Severity.ERR, e.getMessage()));
            }
        }
    }
}
