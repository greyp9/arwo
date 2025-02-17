package io.github.greyp9.arwo.kube.core;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

/**
 * Utility class to abstract away the chosen JSON implementation.  (The parser "com.google.gson" is already available
 * in the kube classpath.)
 */
public final class JsonU {

    private JsonU() {
    }

    public static String toJson(final Object object) {
        final Gson gson = new GsonBuilder().create();
        return gson.toJson(object);
    }

    public static String toJsonPretty(final Object object) {
        final Gson gson = new GsonBuilder().setPrettyPrinting().create();
        return gson.toJson(object);
    }

    public static JsonElement toJsonTree(final Object object) {
        final Gson gson = new GsonBuilder().create();
        return gson.toJsonTree(object);
    }

    public static <T> T fromJson(final String json, final Class<T> classOfT) {
        final Gson gson = new GsonBuilder().create();
        return gson.fromJson(json, classOfT);
    }

    public static JsonElement fromJsonToTree(final String json) {
        return JsonParser.parseString(json);
    }

    public static String toPretty(final JsonElement json) {
        final Gson gson = new GsonBuilder().setPrettyPrinting().create();
        return gson.toJson(json);
    }
}
