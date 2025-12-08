package io.github.greyp9.arwo.core.menu2.test;

import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.model.MenuState;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Properties;
import java.util.logging.Logger;

class MenuTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testBasic() {
        final MenuItem menuItem1 = new MenuItem("alpha", null, null, null, null);
        final MenuItem menuItem2 = new MenuItem("beta", null, null, null, null);
        final MenuItem menu = new MenuItem("menu", null, null, null, null, menuItem1, menuItem2);

        Assertions.assertFalse(menu.isOpen());
        Assertions.assertEquals(2, menu.getMenuItems().size());
        for (MenuItem menuItem : menu.getMenuItems()) {
            Assertions.assertFalse(menuItem.isOpen());
            Assertions.assertEquals(0, menuItem.getMenuItems().size());
            logger.finest(menuItem.getName());
        }
    }

    @Test
    void testApplyState() {
        final MenuItem menuItem1 = new MenuItem("alpha", null, null, "menu/alpha", null);
        final MenuItem menuItem2 = new MenuItem("beta", null, null, "menu/beta", null);
        final MenuItem menu = new MenuItem("menu", null, null, "menu", null, menuItem1, menuItem2);

        final Properties menuStateProperties = new Properties();
        final MenuState menuState = new MenuState(menuStateProperties);
        menuState.applyTo(menu);
        Assertions.assertFalse(menu.isOpen());
        Assertions.assertFalse(menuItem1.isOpen());
        Assertions.assertFalse(menuItem2.isOpen());

        menuStateProperties.setProperty("menu", Boolean.TRUE.toString());
        menuState.applyTo(menu);
        Assertions.assertTrue(menu.isOpen());

        menuStateProperties.setProperty("menu/alpha", Boolean.TRUE.toString());
        menuState.applyTo(menu);
        Assertions.assertTrue(menuItem1.isOpen());
    }
}
