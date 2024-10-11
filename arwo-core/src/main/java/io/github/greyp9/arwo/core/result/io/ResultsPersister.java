package io.github.greyp9.arwo.core.result.io;

import io.github.greyp9.arwo.core.alert.write.AlertWriter;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.result.xml.ResultsWriter;

import java.io.IOException;

public class ResultsPersister {
    private final ResultsContext context;
    private final boolean shouldWrite;

    public ResultsPersister(final ResultsContext context) {
        this.context = context;
        this.shouldWrite = (context.getMetaLink().getFile() != null);
    }

    public final void write(final byte[] bytes) throws IOException {
        if (shouldWrite) {
            StreamU.writeMkdirs(context.getMetaLink().getFile(), bytes);
            final AlertWriter alertWriter = new AlertWriter(context.getBundle(), context.getAlerts());
            alertWriter.write("command.finished", "results.view", context.getMetaLink().getHref());
        }
    }

    public final void write(final Results results) throws IOException {
        if (shouldWrite) {
            new ResultsWriter().writeTo(context.getMetaLink().getFile(), results);
            final AlertWriter alertWriter = new AlertWriter(context.getBundle(), context.getAlerts());
            alertWriter.write("command.finished", "results.view", context.getMetaLink().getHref());
        }
    }
}
