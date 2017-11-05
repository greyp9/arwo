package io.github.greyp9.arwo.core.depend;

import io.github.greyp9.arwo.core.lang.CompareU;

public class Dependency implements Comparable<Dependency> {
    private final String src;
    private final String dest;
    private final String md5;
    private final String sha1;

    public String getSrc() {
        return src;
    }

    public String getDest() {
        return dest;
    }

    public String getMd5() {
        return md5;
    }

    public String getSha1() {
        return sha1;
    }

    public Dependency(String src, String dest, String md5, String sha1) {
        this.src = src;
        this.dest = dest;
        this.md5 = md5;
        this.sha1 = sha1;
    }

    @Override
    public int compareTo(final Dependency dependency) {
        return CompareU.compare(src, dependency.getSrc());
    }
}
