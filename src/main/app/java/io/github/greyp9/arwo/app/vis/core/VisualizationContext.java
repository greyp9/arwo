package io.github.greyp9.arwo.app.vis.core;

import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class VisualizationContext {
    private final File folder;
    private final String context;

    public VisualizationContext(final File folder, final String context) {
        this.folder = folder;
        this.context = context;
    }

    public String iterateMetric(final String metric, final int value) {
        String toMetric = metric;
        final List<String> metrics = getMetrics(folder, context);
        if (!metrics.isEmpty()) {
            final int indexOf = metrics.indexOf(metric);
            if (indexOf >= 0) {
                toMetric = metrics.get((indexOf + value + metrics.size()) % metrics.size());
            } else {
                toMetric = metrics.get(0);
            }
        }
        return toMetric;
    }

    private static List<String> getMetrics(final File folder, final String context) {
        final Collection<String> metrics = new TreeSet<>();
        final File folderHistogram = new File(folder, context);
        final Collection<File> files = new FindInFolderQuery(folderHistogram, PATTERN_FILENAME, false).getFound();
        for (final File file : files) {
            final Matcher matcher = PATTERN_FILENAME.matcher(file.getName());
            if (matcher.matches()) {
                metrics.add(matcher.group(1));  // extract the metric name from the filename
            }
        }
        metrics.remove(context);  // ignore the default filename for the context
        return new ArrayList<>(metrics);
    }

    private static final Pattern PATTERN_FILENAME = Pattern.compile("(.*)\\.(.*)\\.xml");  // filename pattern is known
}
