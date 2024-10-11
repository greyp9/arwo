package io.github.greyp9.arwo.core.metric.histogram2.time;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.Map;

public final class TimeHistogramSerializer {
    private final File folder;
    private final TimeHistogram timeHistogram;

    public TimeHistogramSerializer(final File folder, final TimeHistogram timeHistogram) {
        this.folder = folder;
        this.timeHistogram = timeHistogram;
    }

    public void loadAll() throws IOException {
        final File folderHistogram = new File(folder, timeHistogram.getName());
        final String pattern = String.format("%s.*.xml", timeHistogram.getMetric());
        final FindInFolderQuery query = new FindInFolderQuery(folderHistogram, pattern, false);
        Collection<File> found = query.getFound();
        for (File file : found) {
            toHistogram(timeHistogram, StreamU.read(file));
        }
    }

    public void saveAll() throws IOException {
        final File folderHistogram = new File(folder, timeHistogram.getName());
        final Map<Date, TimeHistogramPage> pages = timeHistogram.getPages();
        for (Map.Entry<Date, TimeHistogramPage> entry : pages.entrySet()) {
            final TimeHistogramPage page = entry.getValue();
            final File file = getFile(folderHistogram, timeHistogram.getMetric(), page.getDateStart());
            StreamU.writeMkdirs(file, toBytes(page));
        }
    }

    public File getFile(final File folderHistogram, final String metric, final Date date) {
        final String filename = String.format("%s.%s.xml", metric, DateX.toFilenameMM(date));
        return new File(folderHistogram, filename);
    }

    public byte[] toBytes(final TimeHistogramPage page) throws IOException {
        final Date dateStart = page.getDateStart();
        final double[] buckets = page.getBuckets();
        final Document document = DocumentU.createDocument(Const.ROOT, Const.URI);
        final Element element = document.getDocumentElement();
        ElementU.addElement(element, Const.DATE, XsdDateU.toXSDZ(dateStart));
        for (double bucket : buckets) {
            ElementU.addElement(element, Const.BUCKET, Double.toString(bucket));
        }
        return DocumentU.toXml(document);
    }

    public void toHistogram(final TimeHistogram histogram, final byte[] bytes) throws IOException {
        Date dateIt = null;
        final Document document = DocumentU.toDocument(bytes);
        final Element element = document.getDocumentElement();
        final Collection<Element> elements = ElementU.getChildren(element);
        for (Element elementIt : elements) {
            final String text = elementIt.getTextContent();
            if (Const.DATE.equals(elementIt.getLocalName())) {
                dateIt = XsdDateU.fromXSDZ(text);
            } else if (Const.BUCKET.equals(elementIt.getLocalName())) {
                histogram.add(dateIt, Double.parseDouble(text));
                dateIt = histogram.incrementCell(dateIt, 1);
            }
        }
    }

    public static class Const {
        private static final String URI = TimeHistogram.class.getSimpleName();
        private static final String ROOT = TimeHistogram.class.getSimpleName();
        private static final String DATE = Date.class.getSimpleName();
        private static final String BUCKET = "bucket";
    }
}
