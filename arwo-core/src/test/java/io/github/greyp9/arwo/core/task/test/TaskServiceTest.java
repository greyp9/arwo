package io.github.greyp9.arwo.core.task.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.task.config.EnvironmentConfig;
import io.github.greyp9.arwo.core.task.config.TaskServiceConfig;
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.task.service.Environment;
import io.github.greyp9.arwo.core.task.service.TaskService;
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;
import io.github.greyp9.arwo.core.value.Value;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.logging.Logger;

public class TaskServiceTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testVanilla() throws IOException, ExecutionException, InterruptedException {
        final TaskServiceConfig config = new TaskServiceConfig("service", 1, null, null);
        logger.info(config.getName());
        final TaskService taskService = new TaskService(config);

        final Task task1 = taskService.submit(new ProcessTask("name", new Date(), "ls", false, null, null));
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
        final Task task2 = taskService.submit(new ProcessTask("name", new Date(), "ls", false, null, dir));
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
            logger.info(String.format("SUBMIT/START/FINISH::%s:%s:%s",
                    XsdDateU.toXSDZMillis(task.getDateSubmit()),
                    XsdDateU.toXSDZMillis(task.getDateStart()),
                    XsdDateU.toXSDZMillis(task.getDateFinish())));
        }
    }

    @Test
    void testEnvironment() throws ExecutionException, InterruptedException, IOException {
        final Date date = new Date();
        final String envKey = "A";
        final String key = "FOO";
        final String value = XsdDateU.toXSDZMillis(date);
        final TaskServiceConfig config = new TaskServiceConfig("service", 1, null, null);
        config.addEnvironment(new EnvironmentConfig(envKey, Collections.singletonMap(key, value)));
        final TaskService taskService = new TaskService(config);
        final Environment environment = new Environment(taskService.getEnv(envKey));
        final Task task = taskService.submit(
                new ProcessTask("task", date, Collections.singletonList("echo $FOO"), true, environment, null));
        task.getFuture().get();
        final ProcessTask processTask = Assertions.assertInstanceOf(ProcessTask.class, task);
        final String stdout = processTask.getStdout().getString();
        Assertions.assertTrue(stdout.contains(value));
    }

    @Test
    void testShell() throws ExecutionException, InterruptedException, IOException {
        final String[] commandArray = { "ls" };
        final TaskServiceConfig config = new TaskServiceConfig("service", 1, null, null);
        final TaskService taskService = new TaskService(config);
        final Task task = taskService.submit(
                new ProcessTask("task", new Date(), Arrays.asList(commandArray), true, null, null));
        final ProcessTask processTask = Assertions.assertInstanceOf(ProcessTask.class, task);
        task.getFuture().get();
        final String stdout = processTask.getStdout().getString();
        logger.info(stdout);
        Assertions.assertTrue(stdout.contains("pom.xml"));
    }


    @Test
    void testPWD() throws ExecutionException, InterruptedException, IOException {
        final TaskServiceConfig config = new TaskServiceConfig("service", 1, null, null);
        final TaskService taskService = new TaskService(config);
        final Task task = taskService.submit(
                new ProcessTask("task", new Date(), Collections.singletonList("pwd"), true, null, null));
        final ProcessTask processTask = Assertions.assertInstanceOf(ProcessTask.class, task);
        task.getFuture().get();
        final String stdout = processTask.getStdout().getString();
        logger.info(stdout);
        Assertions.assertTrue(stdout.contains(SystemU.userDir()));
    }

    @Test
    void testAdditionalStdin() throws ExecutionException, InterruptedException, IOException {
        final Date date = new Date();
        final String value = XsdDateU.toXSDZMillis(date);
        final TaskServiceConfig config = new TaskServiceConfig("service", 1, null, null);
        final TaskService taskService = new TaskService(config);
        final Task task = taskService.submit(
                new ProcessTask("task", date, "read MY_STRING && echo $MY_STRING", true, null, null));
        final ProcessTask processTask = Assertions.assertInstanceOf(ProcessTask.class, task);
        processTask.getStdin().addString(value + "\n");
        task.getFuture().get();
        final String stdout = processTask.getStdout().getString();
        logger.info(stdout);
        Assertions.assertTrue(stdout.contains(value));
    }
}
