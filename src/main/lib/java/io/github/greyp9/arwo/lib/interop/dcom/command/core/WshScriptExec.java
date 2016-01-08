package io.github.greyp9.arwo.lib.interop.dcom.command.core;

import org.jinterop.dcom.common.JIException;
import org.jinterop.dcom.core.IJIComObject;
import org.jinterop.dcom.core.JIVariant;
import org.jinterop.dcom.impls.JIObjectFactory;
import org.jinterop.dcom.impls.automation.IJIDispatch;

public class WshScriptExec {
    private final IJIDispatch dispatch;

    public WshScriptExec(final IJIDispatch dispatch) {
        this.dispatch = dispatch;
    }

    public final IJIDispatch getDispatch() {
        return dispatch;
    }

    public final int getStatus() throws JIException {
        final JIVariant variant = dispatch.get("Status");  // i18n
        return variant.getObjectAsInt();
    }

    public final int getExitCode() throws JIException {
        final JIVariant variant = dispatch.get("ExitCode");  // i18n
        return variant.getObjectAsInt();
    }

    public final int getPID() throws JIException {
        final JIVariant variant = dispatch.get("ProcessID");  // i18n
        return variant.getObjectAsInt();
    }

    public final OutputTextStream getStdin() throws JIException {
        final JIVariant variant = dispatch.get("StdIn");  // i18n
        final IJIComObject comObject = variant.getObjectAsComObject();
        return new OutputTextStream((IJIDispatch) JIObjectFactory.narrowObject(comObject));
    }

    public final InputTextStream getStdout() throws JIException {
        final JIVariant variant = dispatch.get("StdOut");  // i18n
        final IJIComObject comObject = variant.getObjectAsComObject();
        return new InputTextStream((IJIDispatch) JIObjectFactory.narrowObject(comObject));
    }

    public final InputTextStream getStderr() throws JIException {
        final JIVariant variant = dispatch.get("StdErr");  // i18n
        final IJIComObject comObject = variant.getObjectAsComObject();
        return new InputTextStream((IJIDispatch) JIObjectFactory.narrowObject(comObject));
    }

    @SuppressWarnings("unused")
    public final void terminate() throws JIException {
        dispatch.callMethodA("Terminate");  // i18n
    }

    public static final class Factory {
        private Factory() {
        }

        public static WshScriptExec create(final JIVariant... variants) throws JIException {
            WshScriptExec exec = null;
            if ((variants != null) && (variants.length > 0)) {
                final IJIComObject comObject = variants[0].getObjectAsComObject();
                final IJIDispatch dispatch = (IJIDispatch) JIObjectFactory.narrowObject(comObject);
                exec = new WshScriptExec(dispatch);
            }
            return exec;
        }
    }
}
