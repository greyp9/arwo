package io.github.greyp9.arwo.app.ssh.sftp.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolder;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.lib.ganymed.ssh.core.SFTP;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;

public class SFTPResourceView {
    private final SFTPRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final SSHConnectionResource resource;

    public SFTPResourceView(
            final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;
        this.resource = resource;
    }

    public final HttpResponse doGetResource(final String path) throws IOException {
        HttpResponse httpResponse;
        final SFTPDataSource source = new SFTPDataSource(request, resource);
        final Integer type = SFTPFolder.toType(source.lstat(path));
        final boolean isFolder = type.equals(SFTP.S_IFDIR);
        //boolean isFile = (type.equals(SFTP.S_IFREG));
        //boolean isLink = (type.equals(SFTP.S_IFLNK));
        if ((isFolder) && (!path.endsWith(Http.Token.SLASH))) {
            // folder path in application should end with slash
            httpResponse = HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getURI()));
        } else if (isFolder) {
            httpResponse = doGetFolder();
            //} else if (isLink) {
            //    return doGetLink();
            //} else if (isFile) {
            //    return doGetFile();
        } else {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, String.format("%d = lstat()", type)));
            httpResponse = HttpResponseU.to501();
        }
        return httpResponse;
    }

    private HttpResponse doGetFolder() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.HTML)));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // context-specific content
        //new MenuViewApp(httpRequest, userState).doMenu(body, AppMenuFactory.SFTP);
        //new MenuViewApp(httpRequest, userState).doTitle(body, title);
        //if (userState.isToggleProperties()) {
        //    new AppPropertiesView(PROPERTIES_TABLE_ID, userState).addContent(body, null, getFileProperties());
        //}
        new SFTPFolderView(request, userState, resource).addContent(body);
        //new TextFilterView(userState.getLocus().getLocale(), userState.getSubmitID()).
        //        addUI(userState.isToggleTextFilterForm(), body);
        // touch ups
        new AlertsView(userState.getAlerts(), userState.getLocus()).addContentTo(body);
        new StatusBarView(httpRequest, userState.getLocus()).addContentTo(body);
        final AppTitle title = AppTitle.Factory.getResourceLabel(
                request.getHttpRequest(), request.getBundle(), request.getTitlePath());
        new AppHtml(httpRequest).fixup(html, title);
        // package into response
        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    private static class Const {
        private static final String HTML = "io/github/greyp9/arwo/html/xed/xed.html";
    }
}
