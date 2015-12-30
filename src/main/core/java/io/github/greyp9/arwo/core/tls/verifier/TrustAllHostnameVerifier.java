package io.github.greyp9.arwo.core.tls.verifier;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLSession;

public class TrustAllHostnameVerifier implements HostnameVerifier {

    @Override
    public final boolean verify(final String hostname, final SSLSession session) {
        return true;
    }
}
