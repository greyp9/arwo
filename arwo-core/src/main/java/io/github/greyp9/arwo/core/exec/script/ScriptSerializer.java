package io.github.greyp9.arwo.core.exec.script;

import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.date.FilenameFactory;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.result.xml.ResultsXML;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.logging.Logger;

public final class ScriptSerializer extends ResultsXML {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public ScriptSerializer() {
    }

    public ScriptContext read(final File script) throws IOException {
        final Document document = DocumentU.toDocument(StreamU.read(script));
        final Element element = document.getDocumentElement();
        final String context = ElementU.getAttribute(element, A_CONTEXT);
        final String command = ElementU.getAttribute(element, A_COMMAND);
        final Date dateSubmit = XsdDateU.fromXSDZ(ElementU.getAttribute(element, A_SUBMIT));
        final ScriptContext scriptContext = new ScriptContext(context, command, dateSubmit, script.getParentFile());
        scriptContext.setExitValue(NumberU.toInteger(ElementU.getAttribute(element, A_EXIT_VALUE)));
        scriptContext.setDateStart(XsdDateU.fromXSDZ(ElementU.getAttribute(element, A_START)));
        scriptContext.setDateFinish(XsdDateU.fromXSDZ(ElementU.getAttribute(element, A_FINISH)));
        scriptContext.getStdout().addString(ElementU.getTextContent(ElementU.getChild(element, E_STDOUT)));
        scriptContext.getStderr().addString(ElementU.getTextContent(ElementU.getChild(element, E_STDERR)));
        return scriptContext;
    }

    public void write(final ScriptContext script) throws IOException {
        final File persistDir = script.getPersistDir();
        if (persistDir.exists()) {
            final Document document = DocumentU.createDocument(E_RESULTS, NS_RESULTS);
            final Element elementResults = document.getDocumentElement();
            final String exitValue = Value.defaultOnNull(script.getExitValue(), null);
            ElementU.setAttributes(elementResults, NTV.create(
                    A_CONTEXT, script.getContext(),
                    A_COMMAND, script.getCommand(),
                    A_EXIT_VALUE, exitValue,
                    A_SUBMIT, XsdDateU.toXSDZMillis(script.getDateSubmit()),
                    A_START, XsdDateU.toXSDZMillis(script.getDateStart()),
                    A_FINISH, XsdDateU.toXSDZMillis(script.getDateFinish())));
            ElementU.addElement(elementResults, E_STDOUT, script.getStdoutText());
            ElementU.addElement(elementResults, E_STDERR, script.getStderrText());
            final File file = FilenameFactory.getUnused(persistDir, "$DATE.xml", script.getDateStart());
            StreamU.write(file, DocumentU.toXml(document));
        } else {
            logger.info("NO PERSIST DIR");
        }
    }
}
