package io.github.greyp9.arwo.core.text.line.filter.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.metric.histogram.core.TimeHistogram;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.text.filter.TextLineFilter;
import io.github.greyp9.arwo.core.text.line.LineU;
import org.junit.Assume;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LogLineFilterTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testTextLineFilter_Simple() throws IOException {
        final String folderOut = System.getProperty("folderOut");
        final String tzid = System.getProperty("tzid");  // "America/New_York"
        Assume.assumeNotNull(folderOut, tzid);

        final File[] foldersIn = {
                new File(SystemU.userHome(), "Downloads/data1"),
        };

        // skip on existing data (clear output data manually for each run)
        final FindInFolderQuery fileQueryOut = new FindInFolderQuery(new File(folderOut), "*.*", false);
        final Collection<File> filesOut = fileQueryOut.getFound();
        Assume.assumeTrue(filesOut.isEmpty());

        // load queries to be executed on input data
        final Collection<TextQuery> textQueries = new ArrayList<>();
        final String text = UTF8Codec.toString(StreamU.read(
                ResourceU.resolve("io/github/greyp9/arwo/text/queries.txt")));
        final String[] paragraphs = text.split("'''");
        for (String paragraph : paragraphs) {
            if (!paragraph.trim().isEmpty()) {
                final String title = LineU.toLines(paragraph).iterator().next().trim();
                final String expression = paragraph.replace(title, "").trim();
                if (!title.isEmpty()) {
                    final TextFilters textFilters = new TextFilters();
                    textFilters.getExpressions().add(expression);
                    final TextLineFilter filter = new TextLineFilter(textFilters);
                    textQueries.add(new TextQuery(title, filter));
                    logger.info(String.format("ADDED QUERY: [%s]", title));
                }
            }
        }

        // set up input data pattern criteria
        final DateX dateX = new DateX("yyyy-MM-dd HH:mm:ss", TimeZone.getTimeZone(tzid));
        final Pattern pattern = Pattern.compile("(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}).+");

        for (File folderIn : foldersIn) {
            if (folderIn.exists()) {
                final FindInFolderQuery queryIn = new FindInFolderQuery(folderIn, "*.log", false);
                final List<File> filesIn = new ArrayList<>(queryIn.getFound());
                for (File fileIn : filesIn) {
                    logger.info(String.format("[%s] [%s]", folderIn.getName(), fileIn.getName()));
                    // load input data
                    final byte[] bytesIn = StreamU.read(fileIn);
                    for (TextQuery textQuery : textQueries) {
                        // prep for input data processing
                        final TimeHistogram histogram = new TimeHistogram(null, textQuery.getTitle(),
                                folderOut, "PT5M", "PT1H", "PT6H", "P1D", "P14D", "P14D");
                        final byte[] bytesOut = textQuery.getFilter().doFilter(bytesIn, StandardCharsets.UTF_8.name());
                        // filter and process input data
                        final Collection<String> lines = LineU.toLines(UTF8Codec.toString(bytesOut));
                        for (String line : lines) {
                            final Matcher matcher = pattern.matcher(line);
                            if (matcher.matches()) {
                                final Date timestamp = dateX.toDate(matcher.group(1));
                                //histogram.normalize(timestamp);
                                histogram.add(timestamp, 1);
                            }
                        }
                        // output filtered data
                        //final Date dateStartHistogram = histogram.getDateStart();
                        //new TimeHistogramSerializer(histogram, new File(folderOut)).save(dateStartHistogram);
                        //StreamU.write(new File(folderOut, String.format("%s.log", textQuery.getTitle())), bytesOut);
                    }
                }
            }
        }
    }

    private static class TextQuery {
        private final String title;
        private final TextLineFilter filter;

        TextQuery(final String title, final TextLineFilter filter) {
            this.title = title;
            this.filter = filter;
        }

        public String getTitle() {
            return title;
        }

        public TextLineFilter getFilter() {
            return filter;
        }
    }
}
