package io.github.greyp9.arwo.core.task.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.task.config.TaskServiceConfig;
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.task.service.TaskService;
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;
import io.github.greyp9.arwo.core.value.Value;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Logger;

public class TaskServiceTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testVanilla() throws IOException, ExecutionException, InterruptedException {
        final TaskServiceConfig config = new TaskServiceConfig("task-service-1", 1);
        logger.info(config.getName());
        final TaskService taskService = new TaskService(config);

        final Task task1 = taskService.submit(new ProcessTask("name", "ls", null, null));
        int expectedTasks = 1;
        Assertions.assertEquals(expectedTasks, taskService.getTasks().size());
        Assertions.assertEquals(expectedTasks, taskService.getFutures().size());
        Assertions.assertEquals(expectedTasks, taskService.getRunnables().size());
        final ProcessTask processTask1 = Value.as(
                taskService.getTasks().stream().findFirst().orElse(null), ProcessTask.class);
        final Future<?> future1 = processTask1.getFuture();
        final Object result1 = future1.get();
        Assertions.assertNull(result1);  // expect null on normal termination
        Assertions.assertTrue(future1.isDone());
        Assertions.assertNotNull(processTask1);
        Assertions.assertEquals(task1, processTask1);
        final String stdout1 = UTF8Codec.toString(processTask1.getStdout().getBytes());
        Assertions.assertFalse(stdout1.isEmpty());

        final File dir = new File(SystemU.userDir());
        final Task task2 = taskService.submit(new ProcessTask("name", "ls", null, dir));
        ++expectedTasks;
        Assertions.assertEquals(expectedTasks, taskService.getTasks().size());
        Assertions.assertEquals(expectedTasks, taskService.getFutures().size());
        Assertions.assertEquals(expectedTasks, taskService.getRunnables().size());
        final ProcessTask processTask2 = Value.as(taskService.getTasks().stream()
                .filter(t -> t.equals(task2)).findFirst().orElse(null), ProcessTask.class);
        final Future<?> future2 = processTask2.getFuture();
        final Object result2 = future2.get();
        Assertions.assertNull(result2);  // expect null on normal termination
        Assertions.assertTrue(future2.isDone());
        Assertions.assertNotNull(processTask2);
        final String stdout2 = UTF8Codec.toString(processTask2.getStdout().getBytes());
        Assertions.assertEquals(stdout1, stdout2);

        final List<Task> tasks = taskService.getTasks();
        for (Task task : tasks) {
            logger.info(String.format("INVOKE/START/FINISH::%s:%s:%s",
                    XsdDateU.toXSDZMillis(task.getDateInvoke()),
                    XsdDateU.toXSDZMillis(task.getDateStart()),
                    XsdDateU.toXSDZMillis(task.getDateFinish())));
        }
    }
}
