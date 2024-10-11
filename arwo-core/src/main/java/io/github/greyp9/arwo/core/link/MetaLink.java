package io.github.greyp9.arwo.core.link;

import java.io.File;

public class MetaLink {
    private final File file;
    private final String href;

    public final File getFile() {
        return file;
    }

    public final String getHref() {
        return href;
    }

    public MetaLink(final File file, final String href) {
        this.file = file;
        this.href = href;
    }

    public MetaLink(final File folderRoot, final String linkRoot, final String offset) {
        this.file = new File(folderRoot, offset);
        this.href = linkRoot + offset;
    }
}
