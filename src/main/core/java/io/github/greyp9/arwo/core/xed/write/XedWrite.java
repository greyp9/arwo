package io.github.greyp9.arwo.core.xed.write;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpClipClear;
import io.github.greyp9.arwo.core.xed.op.OpClipCopy;
import io.github.greyp9.arwo.core.xed.op.OpClipCut;
import io.github.greyp9.arwo.core.xed.op.OpClipPaste;
import io.github.greyp9.arwo.core.xed.op.OpCreate;
import io.github.greyp9.arwo.core.xed.op.OpFill;
import io.github.greyp9.arwo.core.xed.op.OpOrder;
import io.github.greyp9.arwo.core.xed.op.OpPrune;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

import java.io.IOException;

public class XedWrite {
    private final XedRequest request;

    public XedWrite(final XedRequest request) {
        this.request = request;
    }

    public final String apply(final SubmitToken token, final NameTypeValues httpArguments) throws IOException {
        String location = request.getHttpRequest().getURI();
        final XedNav nav = new XedNav(request.getSession().getXed());
        final Pather pather = new Pather(request.getHttpRequest().getPathInfo());
        final String uri = ((Value.isEmpty(token.getObject2())) ? pather.getRight() : token.getObject2());
        final XedCursor cursor = nav.find(uri);
        if (cursor != null) {
            location = apply(location, cursor, token, httpArguments);
        }
        return location;
    }

    private String apply(final String location, final XedCursor cursor,
                         final SubmitToken token, final NameTypeValues httpArguments) throws IOException {
        // handle form submit (write to session TypeInstance)
        final ValueInstance valueInstance = ValueInstance.create(cursor.getTypeInstance(), httpArguments);
        // redirect to clean up client POST state
        return apply(location, cursor, token, valueInstance);
    }

    @SuppressWarnings({ "PMD.CyclomaticComplexity", "PMD.StdCyclomaticComplexity", "PMD.ModifiedCyclomaticComplexity" })
    private String apply(final String locationIn, final XedCursor cursor,
                         final SubmitToken token, final ValueInstance valueInstance) throws IOException {
        String location = locationIn;
        final Pather pather = new Pather(request.getHttpRequest().getPathInfo());
        final String baseURI = request.getHttpRequest().getBaseURI() + pather.getLeft();
        final String action = token.getAction();
        final String message = request.getBundle().getString("alert.action.not.implemented");
        if (action == null) {
            getClass();  // NOOP - null guard
        } else if (App.Action.CREATE.equals(action)) {
            final XsdTypes xsdTypes = cursor.getXed().getXsdTypes();
            final char[] secret = request.getSecret();
            new OpCreate(secret, xsdTypes).apply(cursor.getParentConcrete().getElement(), valueInstance);
        } else if (App.Action.UPDATE.equals(action)) {
            final XsdTypes xsdTypes = cursor.getXed().getXsdTypes();
            final char[] secret = request.getSecret();
            new OpUpdate(secret, xsdTypes).apply(cursor.getElement(), valueInstance);
            new OpOrder().apply(cursor);
        } else if (App.Action.DELETE.equals(action)) {
            cursor.getXed().delete(cursor.getElement());
            location = baseURI + cursor.getParent().getURI();
        } else if (App.Action.CLONE.equals(action)) {
            final Element clone = cursor.getXed().clone(cursor.getElement());
            final XedCursor cursorClone = new XedNav(cursor.getXed()).find(clone);
            location = baseURI + cursorClone.getURI();
        } else if (App.Action.UP.equals(action)) {
            final Element moveUp = cursor.getXed().moveUp(cursor.getElement());
            final XedCursor cursorMove = new XedNav(cursor.getXed()).find(moveUp);
            location = baseURI + cursorMove.getURI();
        } else if (App.Action.DOWN.equals(action)) {
            final Element moveDown = cursor.getXed().moveDown(cursor.getElement());
            final XedCursor cursorMove = new XedNav(cursor.getXed()).find(moveDown);
            location = baseURI + cursorMove.getURI();
        } else if (App.Action.FILL.equals(action)) {
            new OpFill().apply(cursor);
        } else if (App.Action.PRUNE.equals(action)) {
            new OpPrune().apply(cursor);
        } else if (App.Action.CLIP_CLEAR.equals(action)) {
            new OpClipClear(request.getState().getClipboard()).clear();
        } else if (App.Action.CLIP_CUT.equals(action)) {
            new OpClipCut(request.getState().getClipboard()).cut(cursor);
        } else if (App.Action.CLIP_COPY.equals(action)) {
            new OpClipCopy(request.getState().getClipboard()).copy(cursor);
        } else if (App.Action.CLIP_PASTE.equals(action)) {
            new OpClipPaste(request.getState().getClipboard()).paste(cursor);
        } else {
            request.getAlerts().add(new Alert(Alert.Severity.WARN, message, token.toString()));
        }
        request.getSession().setDateModify(request.getHttpRequest().getDate());
        return location;
    }
}
