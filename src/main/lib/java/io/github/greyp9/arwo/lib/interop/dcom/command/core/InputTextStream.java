package io.github.greyp9.arwo.lib.interop.dcom.command.core;

import org.jinterop.dcom.common.JIException;
import org.jinterop.dcom.core.JIVariant;
import org.jinterop.dcom.impls.automation.IJIDispatch;

public class InputTextStream {
    private final IJIDispatch dispatch;

    public InputTextStream(final IJIDispatch dispatch) {
        this.dispatch = dispatch;
    }

    public final IJIDispatch getDispatch() {
        return dispatch;
    }

    public final boolean atEndOfStream() throws JIException {
        final JIVariant atEndOfStream = dispatch.get("AtEndOfStream");  // i18n
        return atEndOfStream.getObjectAsBoolean();
    }

    public final boolean atEndOfLine() throws JIException {
        final JIVariant atEndOfLine = dispatch.get("AtEndOfLine");  // i18n
        return atEndOfLine.getObjectAsBoolean();
    }

    public final String readAll() throws JIException {
        final JIVariant readAll = dispatch.callMethodA("ReadAll");  // i18n
        return readAll.getObjectAsString().getString();
    }

    public final String readLine() throws JIException {
        final JIVariant readLine = dispatch.callMethodA("ReadLine");  // i18n
        return readLine.getObjectAsString().getString();
    }

    public final String read(final int count) throws JIException {
        final Object[] params = new Object[] { count };
        final JIVariant[] read = dispatch.callMethodA("Read", params);  // i18n
        final boolean isData = (read != null) && (read.length > 0);
        return (isData ? read[0].getObjectAsString().getString() : null);
    }
}
