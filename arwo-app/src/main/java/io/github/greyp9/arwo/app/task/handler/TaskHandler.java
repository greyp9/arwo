package io.github.greyp9.arwo.app.task.handler;

import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.task.service.TaskService;
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;

import java.util.Date;

public interface TaskHandler {

    default ProcessTask getProcessTask(final TaskService taskService, final String name, final Date date) {
        return taskService.getTasks().stream()
                .filter(t -> t.getName().equals(name))
                .filter(t -> t.getDateSubmit().equals(date))
                .filter(t -> t instanceof ProcessTask)
                .map(t -> (ProcessTask) t)
                .findFirst().orElse(null);
    }

    default Task getTask(final TaskService taskService, final String name, final Date date) {
        return taskService.getTasks().stream()
                .filter(t -> t.getName().equals(name))
                .filter(t -> t.getDateSubmit().equals(date))
                .findFirst().orElse(null);
    }
}
