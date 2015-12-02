package io.github.greyp9.arwo.lib.ganymed.ssh.core;

@SuppressWarnings({ "OctalInteger", "PMD.AvoidUsingOctalValues" })
public final class SFTP {

    /**
     * @see ch.ethz.ssh2.SFTPv3FileAttributes
     */
    public static final int S_IFLNK = 0120000;
    public static final int S_IFREG = 0100000;
    public static final int S_IFDIR = 0040000;

    private SFTP() {
    }
}
