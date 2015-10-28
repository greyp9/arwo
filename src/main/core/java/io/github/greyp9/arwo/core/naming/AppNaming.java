package io.github.greyp9.arwo.core.naming;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameAlreadyBoundException;
import javax.naming.NameNotFoundException;
import javax.naming.NamingException;

public final class AppNaming {

    private AppNaming() {
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Context createSubcontext(final String name) {
        try {
            final InitialContext initialContext = new InitialContext();
            return initialContext.createSubcontext(name);
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
            return (Context) initialContext.lookup(name);
        } catch (NamingException e) {
            throw new IllegalStateException(e);
        }
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Object lookup(final Context context, final String name) {
        try {
            return context.lookup(name);
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
