package io.github.greyp9.arwo.kube.view;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.url.URLCodec;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class KubeDocMenuFactory {
    private final String pathRoot;
    private final JsonElement jsonRoot;

    public KubeDocMenuFactory(final String pathRoot, final JsonElement jsonRoot) {
        this.pathRoot = pathRoot;
        this.jsonRoot = jsonRoot;
    }

    public MenuItem create() {
        return createMenuBar(pathRoot, "", UTF16.MENU, jsonRoot);
    }

    private static MenuItem createMenuBar(final String path, final String pathOffset,
                                          final String label, final JsonElement jsonElement) {
        final MenuItem menuItem;
        if (jsonElement.isJsonObject()) {
            menuItem = createMenuItem(path, pathOffset, label, jsonElement.getAsJsonObject());
        } else if (jsonElement.isJsonArray()) {
            menuItem = createMenuItem(path, pathOffset, label, jsonElement.getAsJsonArray());
        } else if (jsonElement.isJsonPrimitive()) {
            menuItem = createMenuItem(path, pathOffset, label);
        } else {
            menuItem = null;
        }
        return menuItem;
    }

    private static MenuItem createMenuItem(final String path, final String pathOffset,
                                           final String label, final JsonObject jsonObject) {
        final List<MenuItem> menuItems = new ArrayList<>();
        menuItems.add(createMenuItem(path, pathOffset + "-/", "[view]"));
        final Set<Map.Entry<String, JsonElement>> entries = jsonObject.entrySet();
        for (Map.Entry<String, JsonElement> entry : entries) {
            final String labelChild = entry.getKey();
            final String pathChild = pathOffset + URLCodec.encodeSafe(labelChild) + "/";
            menuItems.add(createMenuBar(path, pathChild, labelChild, entry.getValue()));
        }
        final String labelUse = label.equals(UTF16.MENU) ? label : label + UTF16.CARAT_RIGHT;
        return new MenuItem(labelUse, App.Target.USER_STATE, App.Action.MENU2,
                path + pathOffset, null, menuItems);
    }

    private static MenuItem createMenuItem(final String path, final String pathOffset,
                                           final String label, final JsonArray jsonArray) {
        final List<MenuItem> menuItems = new ArrayList<>();
        menuItems.add(createMenuItem(path, pathOffset + "-/", "[view]"));
        for (int i = 0; (i < jsonArray.size()); ++i) {
            final String labelChild = Integer.toString(i);
            final String pathChild = pathOffset + URLCodec.encodeSafe(labelChild) + "/";
            menuItems.add(createMenuBar(path, pathChild, labelChild, jsonArray.get(i)));
        }
        final String labelUse = label.equals(UTF16.MENU) ? label : label + UTF16.CARAT_RIGHT;
        return new MenuItem(labelUse, App.Target.USER_STATE, App.Action.MENU2,
                path + pathOffset, null, menuItems);
    }

    private static MenuItem createMenuItem(final String path, final String pathOffset, final String label) {
        return new MenuItem(label, null, App.Action.HREF_ABS, path + pathOffset);
    }
}
