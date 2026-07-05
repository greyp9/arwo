package io.github.greyp9.arwo.core.task.type.process;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Logger;

public class ProcessPersister {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final ProcessTask task;

    public ProcessPersister(final ProcessTask task) {
        this.task = task;
    }

    public final void persist(final File folder) {
        if (folder != null) {
            persistTask(folder);
            persistStream(folder, task.getStderr(), ProcessTask.Const.STREAM_STDERR);
            persistStream(folder, task.getStdout(), ProcessTask.Const.STREAM_STDOUT);
        }
    }

    private void persistTask(final File folder) {
        try {
            final Document document = DocumentU.createDocument(PROCESS, PROCESS_NS);
            final Element documentElement = document.getDocumentElement();
            ElementU.setAttributes(documentElement, NTV.create(
                    Task.Const.FIELD_DATE_START, XsdDateU.toXSDZMillis(task.getDateStart()),
                    Task.Const.FIELD_DATE_FINISH, XsdDateU.toXSDZMillis(task.getDateFinish()),
                    Task.Const.FIELD_EXIT_VALUE, Integer.toString(task.getExitValue())));
            ElementU.addElement(documentElement, ProcessTask.Const.STREAM_STDIN,
                    Value.join(" ", Arrays.asList(task.getCmd())));
            final String filename = String.format(FILENAME_XML,
                    task.getName(), DateX.toFilename(task.getDateInvoke()));
            StreamU.writeMkdirs(new File(folder, filename), DocumentU.toXml(document));
        } catch (IOException e) {
            logger.info(e.getMessage());
        }
    }

    private void persistStream(final File folder, final ByteBuffer byteBuffer, final String stream) {
        try {
            final String filename = String.format(FILENAME_STREAM,
                    task.getName(), DateX.toFilename(task.getDateInvoke()), stream);
            StreamU.writeMkdirs(new File(folder, filename), byteBuffer.getBytes());
        } catch (IOException e) {
            logger.info(e.getMessage());
        }
    }

    public static final String PROCESS = "process";
    public static final String PROCESS_NS = "urn:arwo:process";

    public static final String FILENAME_XML = "%s.%s.xml";
    public static final String FILENAME_STREAM = "%s.%s.%s.txt";
}
