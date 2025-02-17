package io.github.greyp9.arwo.kube.core;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

public class JsonUTest {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Test
    void testCompactSerialize() {
        final NameTypeValue ntv = new NameTypeValue("name", "type", "value");
        final String json = JsonU.toJson(ntv);
        logger.info(json);
        Assertions.assertEquals(NTV, json);
    }

    @Test
    void testCompactDeserialize() {
        final NameTypeValue ntv = JsonU.fromJson(NTV, NameTypeValue.class);
        Assertions.assertEquals("name", ntv.getName());
        Assertions.assertEquals("type", ntv.getType());
        Assertions.assertEquals("value", ntv.getValueS());
    }

    @Test
    void testPrettySerialize() {
        final NameTypeValue ntv = new NameTypeValue("name", "type", "value");
        final String json = JsonU.toJsonPretty(ntv);
        logger.info(json);
        Assertions.assertEquals(NTV_PRETTY, json);
    }

    @Test
    void testToJsonTree() {
        final JsonElement jsonTree = JsonU.fromJsonToTree(NTV);
        final JsonObject jsonObject = Assertions.assertInstanceOf(JsonObject.class, jsonTree);

        final JsonElement name = jsonObject.get("name");
        final JsonPrimitive jsonPrimitive = Assertions.assertInstanceOf(JsonPrimitive.class, name);
        Assertions.assertTrue(jsonPrimitive.isString());
        Assertions.assertEquals("name", jsonPrimitive.getAsString());
    }

    @Test
    void testJsonNav() throws IOException {
        final JsonElement jsonTree = JsonU.fromJsonToTree(UTF8Codec.toString(
                StreamU.read(ResourceU.resolve("io/github/greyp9/arwo/kube/core/data.json"))));
        final JsonObject jsonObject = Assertions.assertInstanceOf(JsonObject.class, jsonTree);
        final JsonElement jsonElementA = jsonObject.get("a");
        final JsonObject jsonObjectA = Assertions.assertInstanceOf(JsonObject.class, jsonElementA);
        final JsonElement nameA = jsonObjectA.get("name");
        final JsonPrimitive jsonPrimitive = Assertions.assertInstanceOf(JsonPrimitive.class, nameA);
        Assertions.assertTrue(jsonPrimitive.isString());
        Assertions.assertEquals("nameA", jsonPrimitive.getAsString());
    }

    public static final String NTV = "{\"name\":\"name\",\"type\":\"type\",\"value\":\"value\"}";
    public static final String NTV_PRETTY =
            "{\n  \"name\": \"name\",\n  \"type\": \"type\",\n  \"value\": \"value\"\n}";
}
