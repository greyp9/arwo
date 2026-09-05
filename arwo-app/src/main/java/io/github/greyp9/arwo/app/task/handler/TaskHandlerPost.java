package io.github.greyp9.arwo.app.task.handler;

import io.github.greyp9.arwo.app.core.handler.AppHandlerPost;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.task.core.Task;
import io.github.greyp9.arwo.core.task.service.TaskService;
import io.github.greyp9.arwo.core.task.type.process.ProcessSignal;
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionStdin;

import java.io.IOException;
import java.util.Date;

public class TaskHandlerPost extends AppHandlerPost implements TaskHandler {
    private final TaskService taskService;

    public TaskHandlerPost(final ServletHttpRequest httpRequest,
                           final AppUserState userState,
                           final TaskService taskService) {
        super(httpRequest, userState);
        this.taskService = taskService;
    }

    @Override
    protected final String applySession(final SubmitToken token,
                                        final NameTypeValues httpArguments,
                                        final String locationIn) throws IOException {
        final Pather patherContext = new Pather(getHttpRequest().getPathInfo());
        final Pather patherTaskID = new Pather(patherContext.getRight());

        final String name = patherContext.getLeftToken();
        final Date date = DateX.fromFilename(patherTaskID.getLeftToken());
        final String action = token.getAction();
        final Alerts alerts = getUserState().getAlerts();
        final Bundle bundle = getUserState().getBundle();

        final Task task = getTask(taskService, name, date);
        final ProcessTask processTask = Value.as(task, ProcessTask.class);
        if (task == null) {
            alerts.add(new Alert(Alert.Severity.INFO, bundle.getString("TaskService.task.notFound")));
        } else if ((App.Action.CANCEL.equals(action)) && (task.getDateStart() == null)) {
            final boolean cancel = task.getFuture().cancel(false);
            if (cancel) {
                task.setDateStart(getHttpRequest().getDate());
                task.setDateFinish(getHttpRequest().getDate());
                task.setExitValue(-1);
            }
        } else if (processTask == null) {
            alerts.add(new Alert(Alert.Severity.INFO, bundle.getString("TaskService.task.notFound")));
        } else if (!processTask.isRunning()) {
            alerts.add(new Alert(Alert.Severity.INFO, bundle.getString("TaskService.process.notRunning")));
        } else if (App.Action.STDIN.equals(action)) {
            final String stdin = new XedActionStdin(getUserState().getXedFactory()).getStdin(httpArguments);
            processTask.getStdin().addString(stdin + "\n");
        } else if (App.Action.SIGNAL.equals(action)) {
            new ProcessSignal(processTask.getPid()).sigint();
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, token.toString()));
        }
        return locationIn;
    }
}
