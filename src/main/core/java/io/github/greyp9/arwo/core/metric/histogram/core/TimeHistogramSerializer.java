package io.github.greyp9.arwo.core.metric.histogram.core;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

public class TimeHistogramSerializer {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final TimeHistogram histogram;
    private final File folder;

    public TimeHistogramSerializer(final TimeHistogram histogram, final File folder) {
        this.histogram = histogram;
        this.folder = folder;
    }

    public byte[] serializeData() {
        byte[] bytes = null;
        try {
            final Date dateStart = histogram.getDateStart();
            final double[] buckets = histogram.getBuckets(0, histogram.getLength());
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

    public void deserializeData(byte[] bytes) {
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
                    histogram.setDateStart(dateIt);
                } else if (Const.BUCKET.equals(elementIt.getLocalName())) {
                    histogram.add(dateIt, Double.parseDouble(text));
                    dateIt = DurationU.add(dateIt, DateU.Const.TZ_GMT, duration);
                }
            }
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
    }

    public void save() {
        try {
            StreamU.write(getFile(), serializeData());
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
    }

    public void load() {
        try {
            deserializeData(StreamU.read(getFile()));
        } catch (IOException e) {
            logger.severe(e.getMessage());
        }
    }

    private File getFile() {
        return new File(folder, String.format("%s.xml", histogram.getName()));
    }

    public static class Const {
        private static final String URI = TimeHistogram.class.getSimpleName();
        private static final String ROOT = TimeHistogram.class.getSimpleName();
        private static final String DATE = Date.class.getSimpleName();
        private static final String BUCKET = "bucket";
    }
}
