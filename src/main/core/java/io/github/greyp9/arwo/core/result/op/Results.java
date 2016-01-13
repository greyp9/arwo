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
    private final Collection<Result> data;

    public final String getCommand() {
        return command;
    }

    public final Interval getInterval() {
        return interval;
    }

    public final Collection<Result> getResults() {
        return data;
    }

    public Results(final String command, final Interval interval) {
        this.command = command;
        this.interval = interval;
        this.data = new ArrayList<Result>();
    }

    public final void add(final String id, final String type, final RowSet rowSet) {
        data.add(new RowSetResult(id, type, rowSet));
    }

    public final void add(final String id, final String type, final String text) {
        data.add(new TextResult(id, type, text));
    }
}
