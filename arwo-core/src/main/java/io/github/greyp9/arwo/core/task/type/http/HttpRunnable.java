package io.github.greyp9.arwo.core.task.type.http;

import io.github.greyp9.arwo.core.cer.CertificateU;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.httpclient.HttpClient;
import io.github.greyp9.arwo.core.httpclient.HttpClientU;
import io.github.greyp9.arwo.core.httpclient.HttpsClient;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.logging.Logger;

public class HttpRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final HttpTask task;

    public HttpRunnable(final HttpTask task) {
        this.task = task;
    }

    @Override
    public final void run() {
        logger.entering(getClass().getName(), Runnable.class.getName());
        try {
            task.setDateStart(new Date());
            task.setExitValue(runInner());
            task.setDateFinish(new Date());
        } catch (IOException | GeneralSecurityException e) {
            logger.severe(e.getMessage());
        }
        logger.exiting(getClass().getName(), Runnable.class.getName());
    }

    private int runInner() throws IOException, GeneralSecurityException {
        final X509Certificate x509 = CertificateU.toX509(StreamU.read(new File(task.getResourceCert())));
        final HttpClient httpClient = new HttpsClient(x509, false);
        final URL url = URI.create(task.getUrl()).toURL();
        final NameTypeValues headersRequest = NTV.create(Http.Header.AUTHORIZATION, HttpClientU.toBasicAuth(
                task.getAuthorization(), System.getProperty(task.getAuthorization()).toCharArray()));
        final HttpRequest httpRequest = new HttpRequest(
                task.getMethod(), url.getFile(), url.getQuery(), headersRequest, null);
        final HttpResponse httpResponse = httpClient.doRequest(url, httpRequest);
        final byte[] responseEntity = StreamU.read(httpResponse.getEntity());
        logger.info(UTF8Codec.toString(responseEntity));
        return httpResponse.isSuccess() ? 0 : 1;
    }
}
