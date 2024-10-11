package io.github.greyp9.arwo.app.local.test;

import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.handler.LFSHandlerGet;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

public class LFSFindTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public final void setUp() throws Exception {
        io.github.greyp9.arwo.core.logging.LoggerU.adjustShort(Logger.getLogger(""));
    }

    @Test
    @Disabled
    public void testFolders() throws Exception {
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
        Assertions.assertNotNull(userState);
        // iterate through lfs context roots
        final Stopwatch stopwatch = new Stopwatch(null);
        final Collection<String> resources = new ArrayList<>();
        resources.add("/arwo/lfs/-/find/tmp/");
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
            Assertions.assertNotNull(httpResponse);
            //logger.info(httpResponse.getHeaders().toString());
            Assertions.assertEquals(HttpURLConnection.HTTP_OK, httpResponse.getStatusCode(), pathInfo);
            // load html into parser
            final ByteArrayInputStream responseStream = httpResponse.getEntity();
            Assertions.assertNotNull(responseStream);
            final byte[] responseBytes = StreamU.read(responseStream);
            final Document document = DocumentU.toDocument(responseBytes);
            final XPather pather = new XPather(document);
            // query HTML for resourceHrefs
            final Collection<String> resourcesThisFolder = new ArrayList<>();
            final List<Element> elements = pather.getElements(
                    "/html/body/div[@class='table']//table[@class='table']//tbody[@class='table']/tr/td[1]/a");
            for (Element element : elements) {
                final String href = element.getAttribute(Html.HREF);
                resourcesThisFolder.add(href);
            }
            // optionally persist response bytes (diagnose)
            if (!SystemU.isTrue()) {
                final String responsePath = SystemU.resolve("~/arwo.html");
                StreamU.write(new File(responsePath), responseBytes);
            }
            int resourcesScanned = 0;
            logger.info(String.format("RESOURCES TO SCAN=%d", resourcesThisFolder.size()));
            // each href should point to an available resource
            for (String resourceThis : resourcesThisFolder) {
                logger.info(resourceThis);
                ++resourcesScanned;
                final String pathInfoThis = URLCodec.decode(resourceThis.replace(baseURI, ""));
                final HttpRequest httpRequestThis = new HttpRequest(
                        Http.Method.GET, resource, null, NTV.create(), null);
                final ServletHttpRequest servletHttpRequestThis = new ServletHttpRequest(
                        httpRequestThis, date, principal, contextPath, servletPath, pathInfoThis);
                final HttpResponse httpResponseThis = new LFSHandlerGet(servletHttpRequestThis, userState).doGetSafe();
                // validate response
                Assertions.assertNotNull(httpResponseThis);
                final Collection<String> excludes = Arrays.asList();
                if (!excludes.contains(resourceThis)) {
                    final Collection statusCodesOK = Arrays.asList(
                            HttpURLConnection.HTTP_OK, HttpURLConnection.HTTP_MOVED_TEMP);
                    Assertions.assertTrue(statusCodesOK.contains(httpResponseThis.getStatusCode()));
                }
            }
            logger.info(String.format("RESOURCES SCANNED=%d", resourcesScanned));
        }
        logger.info(String.format("FOLDERS=%d, ELAPSED=%d", foldersScanned, stopwatch.lap()));
    }
}
