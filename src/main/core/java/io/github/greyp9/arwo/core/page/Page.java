package io.github.greyp9.arwo.core.page;

import io.github.greyp9.arwo.core.lang.MathU;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Properties;

public class Page {
    private final int position;
    private final int count;
    private final Properties properties;

    public Page(final int position, final int count) {
        this(position, count, new Properties());
    }

    public Page(final int position, final int count, final Properties properties) {
        this.position = position;
        this.count = count;
        this.properties = properties;
    }

    public final int getPosition() {
        return position;
    }

    public final int getCount() {
        return count;
    }

    public final Properties getProperties() {
        return properties;
    }

    public final int getFirstUI() {
        return position + 1;
    }

    public final int getLastUI(final int size) {
        final int last = position + count;
        return Math.min(last, size);
    }

    public static final class Factory {
        private Factory() {
        }

        public static Page togglePage(final Page page, final int pageSize) {
            return ((page == null) ? Page.Factory.initPage(pageSize, new Properties()) : null);
        }

        public static Page initPage(final int count, final Properties properties) {
            return new Page(0, count, properties);
        }

        public static Page firstPage(final Page page) {
            return new Page(0, page.getCount(), page.getProperties());
        }

        public static Page lastPage(final Page page) {
            return new Page(Integer.MAX_VALUE, page.getCount(), page.getProperties());
        }

        public static Page prevPage(final Page pageIn) {
            Page page = pageIn;
            if (page != null) {
                final int position = page.getPosition() - page.getCount();
                page = new Page(position, page.getCount(), page.getProperties());
            }
            return page;
        }

        public static Page nextPage(final Page pageIn) {
            Page page = pageIn;
            if (page != null) {
                final int position = page.getPosition() + page.getCount();
                page = new Page(position, page.getCount(), page.getProperties());
            }
            return page;
        }

        public static Page fixPage(final Page pageIn, final int count) {
            Page page = pageIn;
            if (page != null) {
                int position = MathU.bound(0, page.getPosition(), count - 1);
                final int pageCount = Value.defaultOnInvalid(page.getCount(), 0, DEFAULT_PAGE_SIZE);
                position -= (position % pageCount);
                page = new Page(position, pageCount, page.getProperties());
            }
            return page;
        }
    }

    private static final int DEFAULT_PAGE_SIZE = 30;
}
