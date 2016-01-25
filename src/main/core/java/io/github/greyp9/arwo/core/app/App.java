package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.glyph.UTF16;

import javax.xml.namespace.QName;

// i18nf
public final class App {

    private App() {
    }

    public static class Actions {
        public static final String XSD = "io/github/greyp9/arwo/xsd/action/action.xsd";
        public static final String URI_ACTION = "urn:arwo:action";
        public static final String PREFIX_ACTION = "action";

        public static final QName QNAME_COMMAND = new QName(URI_ACTION, Action.COMMAND, PREFIX_ACTION);
        public static final QName QNAME_COMMIT = new QName(URI_ACTION, "commit", PREFIX_ACTION);
        public static final QName QNAME_FILE_EDIT = new QName(URI_ACTION, "fileEdit", PREFIX_ACTION);
        public static final QName QNAME_FILE_NEW = new QName(URI_ACTION, "fileNew", PREFIX_ACTION);
        public static final QName QNAME_FOLDER_NEW = new QName(URI_ACTION, "folderNew", PREFIX_ACTION);
        public static final QName QNAME_FILTER = new QName(URI_ACTION, "filter", PREFIX_ACTION);
        public static final QName QNAME_LOCALE = new QName(URI_ACTION, "locale", PREFIX_ACTION);
        public static final QName QNAME_MAIL = new QName(URI_ACTION, "mail", PREFIX_ACTION);
        public static final QName QNAME_SQL = new QName(URI_ACTION, "sql", PREFIX_ACTION);
        public static final QName QNAME_TEXT_FILTER = new QName(URI_ACTION, "textFilter", PREFIX_ACTION);

        public static final String SUBMIT_COMMAND = "[session][command][{urn:arwo:action}command][command]";
        public static final String SUBMIT_MAIL = "[session][mail][{urn:arwo:action}mail][mail]";
        public static final String SUBMIT_SQL = "[session][sql][{urn:arwo:action}sql][sql]";
    }

    public static class Config {
        public static final String XSD = "io/github/greyp9/arwo/xsd/app/app.xsd";
        public static final QName QNAME_APP = new QName("urn:arwo:app", "app", "app");
        public static final QName QNAME_FAVS = new QName("urn:arwo:app", "favorites", "app");
    }

    public static class Servlet {
        public static final String CACHE = "/cache";
        public static final String CIFS = "/cifs";
        public static final String DASH = "/dash";
        public static final String IMAP = "/imap";
        public static final String JDBC = "/jdbc";
        public static final String LFS = "/lfs";
        public static final String LSH = "/lsh";
        public static final String POP3 = "/pop3";
        public static final String SFTP = "/sftp";
        public static final String SMTP = "/smtp";
        public static final String SSH = "/ssh";
        public static final String WEBDAV = "/webdav";
        public static final String WSH = "/wsh";

        public static final String SETTINGS = "/app";
        public static final String FAVORITES = "/fav";
        public static final String USERS = "/users";
    }

    public static class Header {
        public static final String RESULT = "X-Result";
    }

    public static class Cache {
        public static final String CIFS = "cifs";
        public static final String IMAP = "imap";
        public static final String JDBC = "jdbc";
        public static final String LFS = "lfs";
        public static final String LSH = "lsh";
        public static final String POP3 = "pop3";
        public static final String SMTP = "smtp";
        public static final String SSH = "ssh";
        public static final String DAV = "webdav";
        public static final String WSH = "wsh";
    }

    public static class Realm {
        public static final String XSD = "io/github/greyp9/arwo/xsd/realm/realm.xsd";
        public static final String XML_EMPTY = "io/github/greyp9/arwo/xsd/realm/realm.xml";
        public static final QName QNAME = new QName("urn:arwo:realm", "realm", "realm");
    }

    public static class Meter {
        public static final String XSD = "io/github/greyp9/arwo/xsd/meter/meter.xsd";
        public static final QName QNAME_CRON_JOBS = new QName("urn:arwo:meter", "cronJobs", "cronJobs");
    }

    public static class Bundle {
        public static final String CORE = "io.github.greyp9.arwo.text.core.core";
    }

    public static class Html {
        public static final String UI = "io/github/greyp9/arwo/html/xed/xed.html";
    }

    public static class Naming {
        public static final String APP_STATE = "javax.naming.Name-io.github.greyp9.arwo.app.core.state.AppState";
        public static final String EXECUTOR_SERVICE = "javax.naming.Name-java.util.concurrent.ExecutorService";
    }

    public static class CSS {
        public static final String ACTIVE = "active";
        public static final String ALERT = "alert";
        public static final String ALT = "alt";
        public static final String ATTR_NAME = "attr-name";
        public static final String ATTR_VALUE = "attr-value";
        public static final String BUTTONS = "buttons";
        public static final String CENTER = "center";
        public static final String COMMAND = "command";
        public static final String COMMAND_BODY = "command-body";
        public static final String COMMAND_FOOT = "command-foot";
        public static final String COMMAND_HEAD = "command-head";
        public static final String DIALOG = "dialog";
        public static final String DIFFERENCE = "difference";
        public static final String EMPTY = "empty";
        public static final String ENHANCE = "enhance";
        public static final String EXTRA = "extra";
        public static final String FOOTER = "footer";
        public static final String HEADER = "header";
        public static final String LABEL = "label";
        public static final String LEFT = "left";
        public static final String LEVEL = "level";
        public static final String MENU = "menu";
        public static final String MENUS = "menus";
        public static final String MIN = "min";
        public static final String NEW = "new";
        public static final String NOTIFICATION = "notification";
        public static final String NOTIFICATIONS = "notifications";
        public static final String NUMBER = "number";
        public static final String OLD = "old";
        public static final String PAGE_TITLE = "page-title";
        public static final String RIGHT = "right";
        public static final String ROWSET_RESULT = "rowset-result";
        public static final String STATE = "state";
        public static final String STATUS = "status";
        public static final String STDERR = "stderr";
        public static final String STDIN = "stdin";
        public static final String STDOUT = "stdout";
        public static final String TABLE = "table";
        public static final String TEXT = "text";
        public static final String TEXT_RESULT = "text-result";
        public static final String TEXT_RESULT_BODY = "text-result-body";
        public static final String TEXT_RESULT_HEAD = "text-result-head";
        public static final String TIMESTAMP = "timestamp";
        public static final String TRAILING = "trailing";
    }

    public static class Mode {
        public static final String CREATE_F = "createF";
        public static final String CREATE_D = "createD";
        public static final String EDIT = "edit";
        public static final String DELETE = "delete";
        public static final String VIEW = "view";
        public static final String VIEW_GZ = "viewGZ";
        public static final String VIEW_ZIP = "viewZIP";
        public static final String VIEW_TGZ = "viewTGZ";
        public static final String VIEW_HEX = "viewHex";
        public static final String VIEW_R = "viewR";
    }

    public static class Settings {
        public static final String ALGORITHM = "algorithm";
        public static final String AUTH_PASSWORD = "authPassword";
        public static final String AUTH_PUBLIC_KEY = "authPublicKey";
        public static final String CERTIFICATE = "certificate";
        public static final String COMMAND = "command";
        public static final String COMMENT = "comment";
        public static final String DRIVER_CLASS = "driverClass";
        public static final String ENABLED = "enabled";
        public static final String HOST = "host";
        public static final String JDBC_URL = "jdbcURL";
        public static final String NAME = "name";
        public static final String PORT = "port";
        public static final String PRIVATE_KEY = "privateKey";
        public static final String PROTOCOL = "protocol";
        public static final String PUBLIC_KEY = "publicKey";
        public static final String RESOURCE = "resource";
        public static final String SERVER = "server";
        public static final String SHARE = "share";
        public static final String SQL = "sql";
        public static final String USER = "user";
        public static final String PASSWORD = "password";
    }

    @SuppressWarnings({ "OctalInteger", "PMD.AvoidUsingOctalValues" })
    public static class FS {
        public static final String BYTES = "bytes";
        public static final String FILES = "files";
        public static final String FOLDERS = "folders";
        public static final String SYMLINKS = "symlinks";

        /**
         * @see ch.ethz.ssh2.SFTPv3FileAttributes
         */
        public static final int S_IFLNK = 0120000;
        public static final int S_IFREG = 0100000;
        public static final int S_IFDIR = 0040000;
    }

    public static class Action {
        public static final String ALERT = "alert";
        public static final String CLEAR = "clear";
        public static final String CLOSE = "close";
        public static final String LOAD_REVISION = "loadRevision";
        public static final String MENU = "menu";
        //public static final String NAVIGATE = "navigate";
        public static final String PRETTY = "pretty";
        public static final String RELOAD = "reload";
        public static final String RESET = "reset";
        public static final String SAVE = "save";
        public static final String COMMIT = "commit";
        public static final String TOGGLE = "toggle";
        public static final String VALIDATE = "validate";
        public static final String LOCALE = "locale";
        public static final String UPDATE_LOCALE = "updateLocale";
        public static final String TEXT_FILTER = "textFilter";
        public static final String FILE_UPDATE = "fileUpdate";
        public static final String FILE_CREATE = "fileCreate";
        public static final String FOLDER_CREATE = "folderCreate";
        public static final String COMMAND = "command";
        public static final String SQL = "sql";
        public static final String MAIL = "mail";
        public static final String FILESYSTEM = "filesystem";
        public static final String FIND = "find";
        public static final String HEX_VIEW_PARAM = "hexViewParam";
        public static final String PROPERTIES = "properties";
        public static final String MIME_TYPE = "mimeType";
        public static final String CHARSET = "charset";
        public static final String SELECT = "select";
        public static final String CRON_OFF = "cronOff";
        public static final String CRON_ON = "cronOn";
        public static final String CRON_NOW = "cronNow";
        public static final String ADD_FAV = "addFavorite";
        public static final String SELECT_FAV = "selectFavorite";
        public static final String UI = "ui";
        public static final String XML = "xml";
        public static final String XSD = "xsd";
        public static final String REV = "rev";

        public static final String CREATE = "create";
        public static final String UPDATE = "update";
        public static final String DELETE = "delete";
        public static final String CLONE = "clone";
        public static final String UP = UTF16.ARROW_UP;
        public static final String DOWN = UTF16.ARROW_DOWN;

        public static final String FILL = "fill";
        public static final String PRUNE = "prune";

        public static final String CLIP_CLEAR = "clear";
        public static final String CLIP_CUT = "cut";
        public static final String CLIP_COPY = "copy";
        public static final String CLIP_PASTE = "paste";
    }

    public static class Hex {
        public static final String WIDTH_16 = "16";
        public static final String WIDTH_32 = "32";
        public static final String WIDTH_64 = "64";
    }

    public static class Target {
        public static final String DOCUMENT = "document";
        public static final String SESSION = "session";
        public static final String USER_STATE = "userState";
        public static final String VIEW_STATE = "viewState";
    }

    public static class Post {
        public static final String CD_FILENAME = "Content-Disposition.filename";
        public static final String CD_NAME = "Content-Disposition.name";
        public static final String UPLOAD_FILE = "uploadFile";
    }

    public static class Connection {
        public static final String DATE_LAST = "dateLast";
        public static final String COUNT = "count";
        public static final String MILLIS = "millis";
    }
}
