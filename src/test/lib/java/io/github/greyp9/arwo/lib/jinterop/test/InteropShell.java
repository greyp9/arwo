package io.github.greyp9.arwo.lib.jinterop.test;

import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.command.CommandDone;
import io.github.greyp9.arwo.core.io.command.CommandWork;
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

import java.io.IOException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

public class InteropShell {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final String host;
    private final String user;
    private final String pass;

    public InteropShell(String host, String user, String pass) {
        this.host = host;
        this.user = user;
        this.pass = pass;
    }

    public Collection<Command> runCommands(Collection<String> stdins) throws IOException {
        JISystem.setAutoRegisteration(true);
        JISession session = JISession.createSession("", user, pass);
        session.useSessionSecurity(true);
        try {
            Collection<Command> commands = runCommands(stdins, session);
            JISession.destroySession(session);
            return commands;
        } catch (JIException e) {
            throw new IOException(e);
        }
    }

    private Collection<Command> runCommands(Collection<String> stdins, JISession session)
            throws JIException, UnknownHostException {
        JIProgId progId = JIProgId.valueOf("WScript.Shell");
        JIComServer comServer = new JIComServer(progId, host, session);
        return runCommands(stdins, comServer);
    }

    private Collection<Command> runCommands(Collection<String> stdins, JIComServer comServer) throws JIException {
        IJIComObject unknown = comServer.createInstance();
        IJIComObject comObject = unknown.queryInterface(IJIDispatch.IID);
        IJIDispatch shell = (IJIDispatch) JIObjectFactory.narrowObject(comObject);
        queryShell(shell);
        Collection<Command> commands = runCommands(stdins, shell);
        shell.release();
        return commands;
    }

    private void queryShell(IJIDispatch shell) throws JIException {
        JIVariant variantCurrentDirectory = shell.get("CurrentDirectory");
        String currentDirectory = variantCurrentDirectory.getObjectAsString().getString();
        logger.info(currentDirectory);
    }

    private Collection<Command> runCommands(Collection<String> stdins, IJIDispatch shell) throws JIException {
        Collection<Command> commands = new ArrayList<Command>();
        for (String stdin : stdins) {
            commands.add(runCommand(stdin, shell));
        }
        return commands;
    }

    private Command runCommand(String stdin, IJIDispatch shell) throws JIException {
        Date dateStart = new Date();
        Object[] params = { new JIString(String.format("cmd /c %s", stdin)) };
        JIVariant[] results = shell.callMethodA("Exec", params);
        if ((results == null) || (results.length == 0)) {
            CommandWork commandWork = new CommandWork(null, stdin, null, dateStart, 0);
            return new CommandDone(commandWork, "", "", new Date(), -1);
        } else {
            return processResults(stdin, dateStart, results[0]);
        }
    }

    private Command processResults(String stdin, Date dateStart, JIVariant variant) throws JIException {
        IJIComObject comObject = variant.getObjectAsComObject();
        IJIDispatch results = (IJIDispatch) JIObjectFactory.narrowObject(comObject);
        String stdOut = consumeStream(results.get("StdOut"));
        String stdErr = consumeStream(results.get("StdErr"));
        results.release();
        CommandWork commandWork = new CommandWork(null, stdin, null, dateStart, 0);
        return new CommandDone(commandWork, stdOut, stdErr, new Date(), 0);
    }

    private String consumeStream(JIVariant variant) throws JIException {
        IJIComObject comObject = variant.getObjectAsComObject();
        IJIDispatch stream = (IJIDispatch) JIObjectFactory.narrowObject(comObject);
        StringBuilder buffer = new StringBuilder();
        while (!atEndOfStream(stream)) {
            buffer.append(readAll(stream));
        }
        stream.release();
        return buffer.toString();
    }

    private boolean atEndOfStream(IJIDispatch stream) throws JIException {
        JIVariant atEndOfStream = stream.get("AtEndOfStream");
        return atEndOfStream.getObjectAsBoolean();
    }

    private String readAll(IJIDispatch stream) throws JIException {
        JIVariant readAll = stream.callMethodA("ReadAll");
        return readAll.getObjectAsString().getString();
    }
}
