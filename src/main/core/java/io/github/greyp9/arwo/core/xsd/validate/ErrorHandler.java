package io.github.greyp9.arwo.core.xsd.validate;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.regex.Pattern;

public class ErrorHandler extends org.xml.sax.helpers.DefaultHandler {
    private final Collection<Pattern> excludes;
    private final Collection<String> warnings;
    private final Collection<String> errors;
    private final Collection<String> fatals;

    public ErrorHandler() {
        // https://www.w3.org/Bugs/Public/show_bug.cgi?id=1889
        // http://lists.w3.org/Archives/Public/xmlschema-dev/2007Jun/0027.html
        this(Collections.singleton("InvalidRegex.+''-' is an invalid character range.+"));
    }

    public ErrorHandler(final Collection<String> excludes) {
        super();
        this.excludes = compileWhiteList(excludes);
        this.warnings = new ArrayList<String>();
        this.errors = new ArrayList<String>();
        this.fatals = new ArrayList<String>();
    }

    public final Collection<String> getMessages() {
        final Collection<String> messages = new ArrayList<String>();
        messages.addAll(warnings);
        messages.addAll(errors);
        messages.addAll(fatals);
        return messages;
    }

    public final void warning(final SAXParseException e) throws SAXException {
        if (!checkWhiteList(e, excludes)) {
            warnings.add(message(e, "WARN"));
        }
    }

    public final void error(final SAXParseException e) throws SAXException {
        if (!checkWhiteList(e, excludes)) {
            errors.add(message(e, "ERROR"));
        }
    }

    public final void fatalError(final SAXParseException e) throws SAXException {
        if (!checkWhiteList(e, excludes)) {
            fatals.add(message(e, "FATAL"));
        }
    }

    private static String message(final SAXParseException e, final String level) {
        return String.format("%s [%s,%s] %s", level, e.getLineNumber(), e.getColumnNumber(), e.getMessage());
    }

    private static Collection<Pattern> compileWhiteList(final Collection<String> regexes) {
        final Collection<Pattern> patterns = new ArrayList<Pattern>();
        for (final String regex : regexes) {
            patterns.add(Pattern.compile(regex));
        }
        return patterns;
    }

    private static boolean checkWhiteList(final SAXParseException e, final Collection<Pattern> patterns) {
        boolean evaluate = false;
        for (final Pattern pattern : patterns) {
            evaluate |= pattern.matcher(e.getMessage()).matches();
        }
        return evaluate;
    }
}
