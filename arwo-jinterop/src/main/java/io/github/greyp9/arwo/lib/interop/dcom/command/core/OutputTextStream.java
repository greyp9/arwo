package io.github.greyp9.arwo.lib.interop.dcom.command.core;

import org.jinterop.dcom.common.JIException;
import org.jinterop.dcom.impls.automation.IJIDispatch;

public class OutputTextStream {
    private final IJIDispatch dispatch;

    public OutputTextStream(final IJIDispatch dispatch) {
        this.dispatch = dispatch;
    }

    public final IJIDispatch getDispatch() {
        return dispatch;
    }

    public final void write(final String string) throws JIException {
        final Object[] params = new Object[] { string };
        dispatch.callMethodA("Write", params);  // i18n lib
    }
}
