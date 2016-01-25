package io.github.greyp9.arwo.lib.interop.dcom.command.core;

import org.jinterop.dcom.common.JIException;
import org.jinterop.dcom.common.JISystem;
import org.jinterop.dcom.core.IJIComObject;
import org.jinterop.dcom.core.JIComServer;
import org.jinterop.dcom.core.JIProgId;
import org.jinterop.dcom.core.JISession;
import org.jinterop.dcom.core.JIString;
import org.jinterop.dcom.core.JIVariant;
import org.jinterop.dcom.impls.JIObjectFactory;
import org.jinterop.dcom.impls.automation.IJIDispatch;

import java.net.UnknownHostException;

public class WshShell {
    private final IJIDispatch dispatch;

    public WshShell(final IJIDispatch dispatch) {
        this.dispatch = dispatch;
    }

    public final IJIDispatch getDispatch() {
        return dispatch;
    }

    public final String getCurrentDirectory() throws JIException {
        final JIVariant variant = dispatch.get("CurrentDirectory");  // i18n
        return variant.getObjectAsString().getString();
    }

    public final WshScriptExec exec(final String command) throws JIException {
        final String commandLine = "%COMSPEC% /c " + command;  // i18n lib
        final Object[] params = { new JIString(commandLine) };
        final JIVariant[] variants = dispatch.callMethodA("Exec", params);  // i18n lib
        return WshScriptExec.Factory.create(variants);
    }

    public static final class Factory {
        private Factory() {
        }

        public static JISession createSession(final String domain, final String user, final String password) {
            JISystem.setAutoRegisteration(true);
            final JISession session = JISession.createSession(domain, user, password);
            session.useSessionSecurity(true);
            return session;
        }

        public static void destroySession(final JISession session) throws JIException {
            JISession.destroySession(session);
        }

        public static WshShell createShell(
                final String host, final JISession session) throws JIException, UnknownHostException {
            final JIProgId progId = JIProgId.valueOf("WScript.Shell");
            final JIComServer comServer = new JIComServer(progId, host, session);
            final IJIComObject unknown = comServer.createInstance();
            final IJIComObject comObject = unknown.queryInterface(IJIDispatch.IID);
            final IJIDispatch dispatch = (IJIDispatch) JIObjectFactory.narrowObject(comObject);
            return new WshShell(dispatch);
        }

        public static void releaseShell(final WshShell shell) throws JIException {
            shell.getDispatch().release();
        }
    }
}
