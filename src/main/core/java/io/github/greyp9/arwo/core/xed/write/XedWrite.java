package io.github.greyp9.arwo.core.xed.write;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

import java.io.IOException;

public class XedWrite {
    private final XedRequest request;

    public XedWrite(final XedRequest request) {
        this.request = request;
    }

    public final HttpResponse apply(final SubmitToken token, final NameTypeValues nameTypeValues,
                                    final HttpResponse httpResponseIn) throws IOException {
        HttpResponse httpResponse = httpResponseIn;
        final XedNav nav = new XedNav(request.getSession().getXed());
        final Pather pather = new Pather("/ui" + request.getHttpRequest().getPathInfo());
        final XedCursor cursor = nav.find(pather.getRight());
        if (cursor != null) {
            httpResponse = apply(cursor, token, nameTypeValues);
        }
        return httpResponse;
    }

    private HttpResponse apply(final XedCursor cursor,
                               final SubmitToken token, final NameTypeValues nameTypeValues) throws IOException {
        // handle form submit (write to session TypeInstance)
        final String location = request.getHttpRequest().getURI();
        final ValueInstance valueInstance = ValueInstance.create(cursor.getTypeInstance(), nameTypeValues);
        final String location302 = apply(location, valueInstance, cursor, token);
        // redirect to clean up client POST state
        return HttpResponseU.toHttpResponse302(location302);
    }

    private String apply(final String locationIn, final ValueInstance valueInstance,
                         final XedCursor cursor, final SubmitToken token) throws IOException {
        String location = locationIn;
        final String value = token.getAction();
        if (value == null) {
            getClass();  // NOOP - null guard
        } else if (App.Action.CREATE.equals(value)) {
            cursor.getXed().create(cursor.getParentConcrete().getElement(), valueInstance);
        } else if (App.Action.UPDATE.equals(value)) {
            cursor.getXed().update(cursor.getElement(), valueInstance);
        } else if (App.Action.DELETE.equals(value)) {
            cursor.getXed().delete(cursor.getElement());
            location = request.getHttpRequest().getBaseURI() + cursor.getParent().getURI();
        } else if (App.Action.CLONE.equals(value)) {
            cursor.getXed().clone(cursor.getElement());
        } else if (App.Action.UP.equals(value)) {
            final Element moveUp = cursor.getXed().moveUp(cursor.getElement());
            final XedCursor cursorMove = new XedNav(cursor.getXed()).find(moveUp);
            location = request.getHttpRequest().getBaseURI() + cursorMove.getURI();
        } else if (App.Action.DOWN.equals(value)) {
            final Element moveDown = cursor.getXed().moveDown(cursor.getElement());
            final XedCursor cursorMove = new XedNav(cursor.getXed()).find(moveDown);
            location = request.getHttpRequest().getBaseURI() + cursorMove.getURI();
        }
        return location;
    }
}
