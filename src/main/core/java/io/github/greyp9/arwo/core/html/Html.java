package io.github.greyp9.arwo.core.html;

import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.lang.StringU;

// i18nf
public final class Html {

    public static final String A = "a";
    public static final String ACCESSKEY = "accesskey";
    public static final String ACTION = "action";
    public static final String ALT = "alt";
    public static final String BODY = "body";
    public static final String BUTTON = "button";
    public static final String CAPTION = "caption";
    public static final String CHECKBOX = "checkbox";
    public static final String CHECKED = "checked";
    public static final String CLASS = "class";
    public static final String COLS = "cols";
    public static final String COLSPAN = "colspan";
    public static final String DISABLED = "disabled";
    public static final String DIV = "div";
    public static final String ENCTYPE = "enctype";
    public static final String EXPAND = "expand";  // use this to keep empty elements from collapsing
    public static final String FILE = "file";
    public static final String FORM = "form";
    public static final String H1 = "h1";
    public static final String HREF = "href";
    public static final String HTML_TOKEN = "html";
    public static final String HYPHEN = "-";
    public static final String ID = "id";
    public static final String IMG = "img";
    public static final String INPUT = "input";
    public static final String LI = "li";
    public static final String METHOD = "method";
    public static final String NAME = "name";
    public static final String ON = "on";
    public static final String ONCLICK = "onclick";
    public static final String OPTION = "option";
    public static final String PASSWORD = "password";
    public static final String POST = "post";
    public static final String PRE = "pre";
    public static final String RADIO = "radio";
    public static final String REFID = "refid";
    public static final String ROWS = "rows";
    //public static final String SELECT = "select";
    //public static final String SELECTED = "selected";
    public static final String SIZE = "size";
    public static final String SPAN = "span";
    public static final String SRC = "src";
    public static final String STYLE = "style";
    public static final String SUBMIT = "submit";
    public static final String SUMMARY = "summary";
    public static final String TABLE = "table";
    public static final String TBODY = "tbody";
    public static final String TD = "td";
    public static final String TEXT = "text";
    public static final String TEXTAREA = "textarea";
    public static final String TFOOT = "tfoot";
    public static final String TH = "th";
    public static final String THEAD = "thead";
    public static final String TITLE = "title";
    public static final String TR = "tr";
    public static final String TYPE = "type";
    public static final String UL = "ul";
    public static final String VALUE = "value";

    public static final String EMPTY = "";
    public static final String SPACE = " ";
    public static final String MASK = StringU.create(24, UTF16.BULLET);

    public static final String VALUE_1 = "1";
    public static final String VALUE_2 = "2";
    public static final String VALUE_3 = "3";
    public static final String VALUE_4 = "4";
    public static final String VALUE_6 = "6";

    private Html() {
    }

    public static class XPath {
        public static final String BODY = "/html/body";
    }
}
