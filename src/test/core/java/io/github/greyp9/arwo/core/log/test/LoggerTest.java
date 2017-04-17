package io.github.greyp9.arwo.core.log.test;

import junit.framework.TestCase;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

public class LoggerTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testHierarchy() throws Exception {
        logger.info("BEGIN");
        // iterate through hierarchy from current logger
        Logger loggerIt = logger;
        while (loggerIt != null) {
            final Level level = loggerIt.getLevel();
            if (level != null) {
                logger.info(level.getName());
            } else {
                logger.info("null");
            }
            loggerIt = loggerIt.getParent();
        }
        logger.info("END");
    }

    public void testLevels() throws Exception {
        logger.finest("finest");
        logger.finer("finer");
        logger.fine("fine");
        logger.config("config");
        logger.info("info");
        logger.warning("warning");
        logger.severe("severe");
    }

    public void testHandlers() throws Exception {
        final Handler[] handlers = logger.getHandlers();
        for (Handler handler : handlers) {
            logger.info(handler.toString());
        }
    }
}
