package io.github.greyp9.arwo.app.naming.servlet;

import io.github.greyp9.arwo.core.naming.AppNaming;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import java.util.logging.Logger;

public class NamingServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = -8745819191476717538L;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        // leaves behind a arwo.core.logging.FormatterX
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(getClass().getName()));
        Logger.getLogger(getClass().getName()).info("init()");
        final String contextPath = getServletContext().getContextPath();
        AppNaming.createSubcontext(contextPath);
    }

    @Override
    public final void destroy() {
        final String contextPath = getServletContext().getContextPath();
        AppNaming.destroySubcontext(contextPath);
        Logger.getLogger(getClass().getName()).info("destroy()");
        super.destroy();
    }
}
