package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.io.StreamU;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.logging.Logger;

public final class TimeHistogramSerializerFS extends TimeHistogramSerializer {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void save(final TimeHistogram histogram, final Date dateStart) {
        try {
            final TimeHistogramPage page = histogram.getHistogramPage(dateStart);
            final String file = getFile(histogram, dateStart);
            StreamU.writeMkdirs(new File(file), toBytes(page));
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
    }

    @Override
    public void load(final TimeHistogram histogram, final Date dateStart) {
        try {
            final String file = getFile(histogram, dateStart);
            final byte[] bytes = StreamU.read(new File(file));
            toHistogram(histogram, bytes);
        } catch (IOException e) {
            logger.fine(e.getMessage());  // there may not be a backing file; this is ok
        }
    }
}
