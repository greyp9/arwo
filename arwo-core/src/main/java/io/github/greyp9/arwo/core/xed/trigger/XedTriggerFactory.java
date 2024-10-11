package io.github.greyp9.arwo.core.xed.trigger;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.Logger;

public final class XedTriggerFactory {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public XedTrigger createTrigger(final String className) {
        XedTrigger trigger = null;
        try {
            if (className != null) {
                final Class<?> c = Class.forName(className);
                final Constructor<?> ctor = c.getConstructor();
                trigger = (XedTrigger) ctor.newInstance();
            }
        } catch (ClassNotFoundException e) {
            logger.warning(e.getMessage());
        } catch (NoSuchMethodException e) {
            logger.warning(e.getMessage());
        } catch (IllegalAccessException e) {
            logger.warning(e.getMessage());
        } catch (InstantiationException e) {
            logger.warning(e.getMessage());
        } catch (InvocationTargetException e) {
            logger.warning(e.getMessage());
        }
        return trigger;
    }
}
