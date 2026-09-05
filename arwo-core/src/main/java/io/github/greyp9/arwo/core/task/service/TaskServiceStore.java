package io.github.greyp9.arwo.core.task.service;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.find.FindInFolderQuery;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.buffer.ByteBufferFile;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public final class TaskServiceStore {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final TaskService taskService;

    public TaskServiceStore(final TaskService taskService) {
        this.taskService = taskService;
    }

    public void load() {
        final File folderPersist = taskService.getFolderPersist();
        final Collection<File> files = (folderPersist == null) ? Collections.emptyList()
                : new FindInFolderQuery(folderPersist, "*.xml", false).getFound().stream()
                  .sorted(Comparator.comparing(File::lastModified)).collect(Collectors.toList());
        logger.info(String.format("%s=%d", taskService.getName(), files.size()));
        for (File file : files) {
            Optional.ofNullable(toProcessTask(file)).ifPresent(t -> taskService.getTasks().add(t));
        }
    }

    private ProcessTask toProcessTask(final File file) {
        ProcessTask processTask = null;
        try {
            processTask = toProcessTask(DocumentU.toDocument(StreamU.read(file)));
        } catch (IOException e) {
            logger.warning(e.getMessage());
        }
        return processTask;
    }

    private ProcessTask toProcessTask(final Document document) {
        final Element element = document.getDocumentElement();
        final String name = ElementU.getAttribute(element, Task.Const.FIELD_NAME);
        final Element command = ElementU.getChild(element, ProcessTask.Const.FIELD_COMMAND);
        final String commandText = ElementU.getTextContent(command);
        final Date dateSubmit = XsdDateU.fromXSDZ(ElementU.getAttribute(element, Task.Const.FIELD_DATE_SUBMIT));
        final File folderPersist = taskService.getFolderPersist();

        final ProcessTask processTask = new ProcessTask(
                name, dateSubmit, Collections.singletonList(commandText), false, null, null,
                new ByteBufferFile(toStreamFile(folderPersist, dateSubmit, ProcessTask.Const.STREAM_STDOUT)),
                new ByteBufferFile(toStreamFile(folderPersist, dateSubmit, ProcessTask.Const.STREAM_STDERR)));
        processTask.setPid(NumberU.toLong(ElementU.getAttribute(element, ProcessTask.Const.FIELD_PID)));
        processTask.setDateSubmit(dateSubmit);
        processTask.setDateStart(XsdDateU.fromXSDZ(ElementU.getAttribute(element, Task.Const.FIELD_DATE_START)));
        processTask.setDateFinish(XsdDateU.fromXSDZ(ElementU.getAttribute(element, Task.Const.FIELD_DATE_FINISH)));
        processTask.setExitValue(NumberU.toInteger(ElementU.getAttribute(element, ProcessTask.Const.FIELD_EXIT_VALUE)));
        return processTask;
    }

    public static File toFile(final File folderPersist, final Date dateSubmit) {
        final String filename = String.format(FILENAME_XML, DateX.toFilename(dateSubmit));
        return new File(folderPersist, filename);
    }

    public static File toStreamFile(final File folderPersist, final Date dateSubmit, final String stream) {
        final String filename = String.format(FILENAME_STREAM, DateX.toFilename(dateSubmit), stream);
        return new File(folderPersist, filename);
    }

    public static final String FILENAME_XML = "%s.xml";
    public static final String FILENAME_STREAM = "%s.%s.txt";
}
