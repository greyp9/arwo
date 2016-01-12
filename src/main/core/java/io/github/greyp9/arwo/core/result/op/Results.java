package io.github.greyp9.arwo.core.result.op;

import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.result.type.Result;
import io.github.greyp9.arwo.core.result.type.rowset.RowSetResult;
import io.github.greyp9.arwo.core.result.type.text.TextResult;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.util.ArrayList;
import java.util.Collection;

public class Results {
    private final String command;
    private final Interval interval;
    private final Collection<Result> results;

    public String getCommand() {
        return command;
    }

    public Interval getInterval() {
        return interval;
    }

    public Collection<Result> getResults() {
        return results;
    }

    public Results(String command, Interval interval) {
        this.command = command;
        this.interval = interval;
        this.results = new ArrayList<Result>();
    }

    public void add(String id, String type, RowSet rowSet) {
        results.add(new RowSetResult(id, type, rowSet));
    }

    public void add(String id, String type, String text) {
        results.add(new TextResult(id, type, text));
    }
}
