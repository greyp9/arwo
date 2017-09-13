package io.github.greyp9.arwo.core.naming;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameAlreadyBoundException;
import javax.naming.NameNotFoundException;
import javax.naming.NamingException;
import java.util.logging.Logger;

public final class AppNaming {
    private static final Logger LOGGER = Logger.getLogger(AppNaming.class.getName());

    private AppNaming() {
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Context createSubcontext(final String name) {
        try {
            final InitialContext initialContext = new InitialContext();
            final Context subcontext = initialContext.createSubcontext(name);
            LOGGER.finest(String.format("createSubcontext()/%s/%s", name, subcontext));
            return subcontext;
        } catch (NameAlreadyBoundException e) {
            return lookupSubcontext(name);
        } catch (NamingException e) {
            throw new IllegalStateException(e);
        }
    }

    public static void destroySubcontext(final String name) {
        try {
            final InitialContext initialContext = new InitialContext();
            initialContext.destroySubcontext(name);
        } catch (NamingException e) {
            throw new IllegalStateException(e);
        }
    }

    public static void bind(final Context context, final String name, final Object o) {
        try {
            context.bind(name, o);
        } catch (NamingException e) {
            throw new IllegalStateException(e);
        }
    }

    public static void unbind(final Context context, final String name) {
        try {
            if (lookup(context, name) != null) {
                context.unbind(name);
            }
        } catch (NamingException e) {
            throw new IllegalStateException(e);
        }
    }

    public static Context lookupSubcontext(final String name) {
        try {
            final InitialContext initialContext = new InitialContext();
            final Object lookup = initialContext.lookup(name);
            LOGGER.finest(String.format("lookupSubcontext()/%s/%s", name, lookup));
            return (Context) lookup;
        } catch (NamingException e) {
            throw new IllegalStateException(e);
        }
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Object lookup(final Context context, final String name) {
        try {
            return ((context == null) ? null : context.lookup(name));
        } catch (NameNotFoundException e) {
            return null;
        } catch (NamingException e) {
            throw new IllegalStateException(e);
        }
    }

    public static Object lookupQ(final Context context, final String name) {
        try {
            return context.lookup(name);
        } catch (NameNotFoundException e) {
            throw new IllegalStateException(e);
        } catch (NamingException e) {
            throw new IllegalStateException(e);
        }
    }

    public static Object lookup(final String contextName, final String name) {
        return lookup(lookupSubcontext(contextName), name);
    }

    public static Object lookupQ(final String contextName, final String name) {
        return lookupQ(lookupSubcontext(contextName), name);
    }
}
