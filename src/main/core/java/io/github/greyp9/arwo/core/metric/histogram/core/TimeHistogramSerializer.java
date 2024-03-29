package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

public abstract class TimeHistogramSerializer {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public final byte[] toBytes(final TimeHistogramPage page) {
        byte[] bytes = null;
        try {
            final Date dateStart = page.getDateStart();
            final double[] buckets = page.getBuckets(0, page.getPageSize());
            final Document document = DocumentU.createDocument(Const.ROOT, Const.URI);
            final Element element = document.getDocumentElement();
            ElementU.addElement(element, Const.DATE, XsdDateU.toXSDZ(dateStart));
            for (double bucket : buckets) {
                ElementU.addElement(element, Const.BUCKET, Double.toString(bucket));
            }
            bytes = DocumentU.toXml(document);
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
        return bytes;
    }

    public final void toHistogram(final TimeHistogram histogram, final byte[] bytes) {
        try {
            Date dateIt = null;
            final String duration = DurationU.durationXSD(histogram.getDurationCell());
            final Document document = DocumentU.toDocument(bytes);
            final Element element = document.getDocumentElement();
            final Collection<Element> elements = ElementU.getChildren(element);
            for (Element elementIt : elements) {
                final String text = elementIt.getTextContent();
                if (Const.DATE.equals(elementIt.getLocalName())) {
                    dateIt = XsdDateU.fromXSDZ(text);
                    //histogram.setDateStart(dateIt);
                } else if (Const.BUCKET.equals(elementIt.getLocalName())) {
                    histogram.add(dateIt, Double.parseDouble(text));
                    dateIt = DurationU.add(dateIt, DateU.Const.TZ_GMT, duration);
                }
            }
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
    }

    /**
     * Merge in-memory data with persisted data.
     *
     * @param histogram the container for histogram data to be updated
     */
    public final void update(final TimeHistogram histogram) {
        histogram.getHistogramPages().keySet().forEach(date -> {
            load(histogram, date);
            save(histogram, date);
        });
    }

    public abstract void save(TimeHistogram histogram, Date date);

    public abstract void load(TimeHistogram histogram, Date date);

    public final String getFile(final TimeHistogram histogram, final Date date) {
        final String name = (histogram.getMetric() == null) ? histogram.getName() : histogram.getMetric();
        final String filename = (date == null)
                ? String.format("%s.xml", name)
                : String.format("%s.%s.xml", name, DateX.toFilenameMM(date));
        return new File(histogram.getFolder(), filename).getPath();
    }

    public static class Const {
        private static final String URI = TimeHistogram.class.getSimpleName();
        private static final String ROOT = TimeHistogram.class.getSimpleName();
        private static final String DATE = Date.class.getSimpleName();
        private static final String BUCKET = "bucket";
    }
}
