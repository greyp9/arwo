package io.github.greyp9.arwo.core.glyph;

public final class UTF16 {
    public static final String ARROW_DOWN = "\u25bc";  // ▼
    public static final String ARROW_FIRST = "\u25c0\u25c0";
    public static final String ARROW_LAST = "\u25b6\u25b6";
    public static final String ARROW_LEFT = "\u25c0";
    public static final String ARROW_RIGHT = "\u25b6";
    public static final String ARROW_UP = "\u25b2";  // ▲
    public static final String CHECKBOX_FALSE = "\u2610";  // ☐
    public static final String CHECKBOX_TRUE = "\u2611";  // ☑
    public static final String CLOSE = "X";
    public static final String CONNECTED = "\u2282\u2283";  // ⊂⊃
    public static final String DISCONNECTED = "\u2283\u2282";  // ⊃⊂
    public static final String DOCUMENT = "\u2637"; // "\u23cd";  // "\u2395";  // ⎕
    public static final String FAVORITE = "\u2764";  // ❤
    public static final String GO_BACK = "\u232b";  // ⌫
    public static final String HOME = "\u23cf";  // "\u2302";  // ⌂⏏
    public static final String HOURGLASS = "\u231b";  // ⌛
    public static final String ICON_FILE = "\u23cd";  // \u25a6\u2637 ▦☷
    public static final String ICON_FOLDER = "\u2339";  // ◳ \u25f3 "\ud83d\udcc1";
    public static final String ICON_SYMLINK = "\u21b7";  // "\u25e5";
    public static final String MENU = "\u2630";  // "\u2261"
    public static final String NBSP = "\u00a0";
    public static final String PAUSE = "\u25ae\u25ae";
    public static final String PLAY = "\u25b6";
    //public static final String STATUS_FAIL = "\u2718";
    //public static final String STATUS_OK = "\u2713";
    //public static final String SAVE = "\u26c1";
    public static final String STOP = "\u25fc";

    public static final String DOCUMENT_BRACKETS = String.format("[%s]", DOCUMENT);

    public static final String TABLE_BASELINE = String.format("[ %s %s ]", DOCUMENT, DOCUMENT);
    public static final String TABLE_CONNECT = String.format("[ %s ]", CONNECTED);
    public static final String TABLE_DISCONNECT = String.format("[ %s ]", DISCONNECTED);
    public static final String TABLE_PAGE = String.format("[ %s %s ]", ARROW_LEFT, ARROW_RIGHT);
    public static final String TABLE_RESET = String.format("[ %s ]", GO_BACK);

    public static final String COLUMN_FILTER = String.format("[%s]", HOURGLASS);
    public static final String COLUMN_HIDE = String.format("[%s]", "X");
    public static final String COLUMN_SORT = String.format("[%s%s]", ARROW_UP, ARROW_DOWN);

    private UTF16() {
    }
}
