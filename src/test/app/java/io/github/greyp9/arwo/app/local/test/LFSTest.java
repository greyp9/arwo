package io.github.greyp9.arwo.app.local.test;

import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.handler.LFSHandlerGet;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.time.Stopwatch;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

public class LFSTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    public void testFolderRecurse() throws Exception {
        // setup application context
        final String contextPath = "/arwo";
        final String servletPath = App.Servlet.LFS;
        final String baseURI = contextPath + servletPath;
        final AppState appState = new AppState(contextPath);
        final File userHome = AppFolder.getWebappRoot(appState.getContextPath());
        final AppPrincipal principal = new AppPrincipal("arwo", CollectionU.toCollection("*"));
        final Date date = new Date();
        final String submitID = Integer.toHexString(hashCode());
        final DateX dateX = new DateX(HttpDateU.Const.DEFAULT, DateU.Const.TZ_GMT);
        final Locus locus = new Locus(Locale.getDefault(), dateX);
        final AppUserState userState = new AppUserState(appState, principal, date, userHome, submitID, locus);
        Assert.assertNotNull(userState);
        // iterate through lfs context roots
        final Stopwatch stopwatch = new Stopwatch(null);
        final Collection<String> resources = new ArrayList<String>();
        resources.add("/arwo/lfs/-/view/arwo/");
        int foldersScanned = 0;
        while (!resources.isEmpty()) {
            final String resource = resources.iterator().next();
            resources.remove(resource);
            logger.info(resource);
            ++foldersScanned;
            // use LFS business logic to return HTML representation of this folder
            //io.github.greyp9.irby.core.http11.servlet25.Http11ServletRequest.getPathInfo()
            final String pathInfo = URLCodec.decode(resource.replace(baseURI, ""));
            final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, resource, null, NTV.create(), null);
            final ServletHttpRequest servletHttpRequest = new ServletHttpRequest(
                    httpRequest, date, principal, contextPath, servletPath, pathInfo);
            final HttpResponse httpResponse = new LFSHandlerGet(servletHttpRequest, userState).doGetSafe();
            // validate response
            Assert.assertNotNull(httpResponse);
            Assert.assertEquals(HttpURLConnection.HTTP_OK, httpResponse.getStatusCode());
            // load html into parser
            final ByteArrayInputStream responseStream = httpResponse.getEntity();
            Assert.assertNotNull(responseStream);
            final byte[] responseBytes = StreamU.read(responseStream);
            final Document document = DocumentU.toDocument(responseBytes);
            final XPather pather = new XPather(document);
            // query HTML for child folder elements
            final List<Element> elements = pather.getElements(
                    "/html/body/div[@class='table']//table[@class='table']//tbody[@class='table']/tr/td[1]/a");
            for (Element element : elements) {
                // if this is a child folder, add it to the collection to be scanned
                final String text = element.getTextContent();
                if (UTF16.ICON_FOLDER.equals(text)) {
                    final String href = element.getAttribute(Html.HREF);
                    resources.add(href);
                }
            }
            if (!SystemU.isTrue()) {
                final String responsePath = SystemU.resolve("~/arwo.html");
                StreamU.write(new File(responsePath), responseBytes);
            }
        }
        logger.info(String.format("FOLDERS=%d, ELAPSED=%d", foldersScanned, stopwatch.lap()));
    }
}
