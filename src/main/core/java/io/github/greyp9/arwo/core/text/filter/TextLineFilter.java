package io.github.greyp9.arwo.core.text.filter;

import io.github.greyp9.arwo.core.expr.Grammar;
import io.github.greyp9.arwo.core.expr.Tree;
import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Collection;

public class TextLineFilter {
    private final Collection<String> includes;
    private final Collection<String> excludes;
    //private final Collection<String> expressions;
    private final Tree tree;

    public TextLineFilter(final TextFilters textFilters) {
        this.includes = textFilters.getIncludes();
        this.excludes = textFilters.getExcludes();
        //this.expressions = textFilters.getExpressions();
        final String expression = textFilters.getExpressions().iterator().next();
        final Grammar grammar = new Grammar(expression);
        this.tree = (expression == null) ? null : grammar.createTree();
    }

    public final byte[] doFilter(final byte[] bytes, final String encoding) throws IOException {
        final BufferedReader reader = new BufferedReader(
                new InputStreamReader(new ByteArrayInputStream(bytes), encoding));
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(os, encoding));
        while (true) {
            final String line = reader.readLine();
            if (line == null) {
                break;
            } else if (matchesTextFilters(line)) {
                writer.write(line);
                writer.write(SystemU.eol());
            }
        }
        writer.flush();
        writer.close();
        return os.toByteArray();
    }

    private boolean matchesTextFilters(final String line) {
        boolean matches = true;
        if (tree == null) {
            for (final String include : includes) {
                if ((matches) && (!line.contains(include))) {
                    matches = false;
                }
            }
            for (final String exclude : excludes) {
                if ((matches) && (line.contains(exclude))) {
                    matches = false;
                }
            }
        } else {
            matches = tree.evaluate(line);
        }
        return matches;
    }
}
