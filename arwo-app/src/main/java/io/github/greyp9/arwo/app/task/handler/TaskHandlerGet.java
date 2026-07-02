package io.github.greyp9.arwo.app.task.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.task.view.TaskServiceView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.menu2.core.MenuSession;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.task.service.TaskService;
import io.github.greyp9.arwo.core.task.type.process.ProcessTask;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Collections;

public class TaskHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final TaskService taskService;

    public TaskHandlerGet(final ServletHttpRequest httpRequest,
                          final AppUserState userState,
                          final TaskService taskService) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.taskService = taskService;
    }

    public final HttpResponse doGet() throws IOException {
        final HttpResponse httpResponse;
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        final Pather patherName = new Pather(pathInfo);
        final Pather patherDate = new Pather(patherName.getRight());
        final Pather patherStream = new Pather(patherDate.getRight());
        final String name = patherName.getLeftToken();
        final String date = patherDate.getLeftToken();
        final String stream = patherStream.getLeftToken();
        if (pathInfo == null) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else if (!Value.isData(name, date, stream)) {
            httpResponse = doGetList();
        } else {
            httpResponse = doGetStream(name, date, stream);
        }
        return httpResponse;
    }

    public final HttpResponse doGetList() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);
        // context-specific content
        final String labelContext = Value.wrap("[", "]", taskService.getName());
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(), labelContext);
        addMenus(header);
        new TaskServiceView(httpRequest, userState, taskService).addContent(content);
        return new AppHtmlView(httpRequest, userState, appTitle)
                .title(header)
                .actionRefresh(header)
                .actionTextExpression(header)
                .alerts(header)
                .statusBar(footer)
                .appHtml(html)
                .toHttpResponse(html);
    }

    private void addMenus(final Element header) {
        final MenuItem menu = new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU2, MENU_KEY, null,
                new MenuSession().toMenuItem(PathU.toPath(MENU_KEY, App.Target.SESSION)))
                .applyFrom(userState.getMenuSystemState());
        new MenuHtml(httpRequest, userState.getBundle(), userState.getSubmitID(), STYLE_HOME)
                .addTo(header, true, "m", Collections.singletonList(menu));
    }

    private static final String MENU_KEY = "/menu2/cron";
    private static final String STYLE_HOME = "background-color: brown; color: white;";

    private HttpResponse doGetStream(final String name, final String date, final String stream) throws IOException {
        final HttpResponse httpResponse;
        final ProcessTask task = taskService.getTasks().stream()
                .filter(t -> t.getName().equals(name))
                .filter(t -> t.getDateInvoke().equals(DateX.fromFilename(date)))
                .filter(t -> t instanceof ProcessTask)
                .map(t -> (ProcessTask) t)
                .findFirst().orElse(null);
        if (task == null) {
            httpResponse = HttpResponseU.to302(httpRequest.getBaseURI());
        } else if ("stderr".equals(stream)) {
            httpResponse = doGetStream(task.getStderr());
        } else if ("stdout".equals(stream)) {
            httpResponse = doGetStream(task.getStdout());
        } else {
            httpResponse = HttpResponseU.to302(httpRequest.getBaseURI());
        }
        return httpResponse;
    }

    private HttpResponse doGetStream(final ByteBuffer byteBuffer) throws IOException {
        final byte[] payload = byteBuffer.getBytes();
        final FileMetaData metaData = new FileMetaData(null, payload.length, System.currentTimeMillis(), false);
        return HttpResponseU.to200(new MetaFile(
                metaData, Http.Mime.TEXT_PLAIN_UTF8, new ByteArrayInputStream(payload)));
    }
}
