package io.github.greyp9.arwo.app.json.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.Value;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONPointer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.logging.Logger;

public class JsonTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testFromString() {
        final JSONObject jsonObject = new JSONObject("{ \"abc\" : \"def\" }");
        logger.info(String.format("json: %s", jsonObject));
        Assertions.assertEquals("def", jsonObject.getString("abc"));
    }

    @Test
    void testObjectFromResource() throws IOException {
        final byte[] bytes = StreamU.read(ResourceU.resolve("json/test1.json"));
        final JSONObject jsonObject = new JSONObject(UTF8Codec.toString(bytes));
        logger.info(String.format("json: %s", jsonObject));
        Assertions.assertEquals("def", jsonObject.getString("abc"));
    }

    @Test
    void testObjectComplexFromResource() throws IOException {
        final byte[] bytes = StreamU.read(ResourceU.resolve("json/test2.json"));
        final JSONObject jsonObject = new JSONObject(UTF8Codec.toString(bytes));
        final int keysExpected = 3;
        Assertions.assertEquals(keysExpected, jsonObject.keySet().size());
        Assertions.assertEquals("a", jsonObject.getString("alpha"));
        final JSONObject beta = jsonObject.getJSONObject("beta");
        Assertions.assertEquals(1, beta.getNumber("b1"));
        final JSONArray gamma = jsonObject.getJSONArray("gamma");
        Assertions.assertEquals(2, gamma.length());
    }

    @Test
    void testPointer() throws IOException {
        final byte[] bytes = StreamU.read(ResourceU.resolve("json/test2.json"));
        final JSONObject jsonObject = new JSONObject(UTF8Codec.toString(bytes));
        final Object o1 = new JSONPointer("/alpha").queryFrom(jsonObject);
        Assertions.assertInstanceOf(String.class, o1);
        final String a = Value.as(o1, String.class);
        Assertions.assertEquals("a", a);
        final Object o2 = new JSONPointer("/beta").queryFrom(jsonObject);
        Assertions.assertInstanceOf(JSONObject.class, o2);
        final Object o2b = new JSONPointer("/beta/b1").queryFrom(jsonObject);
        Assertions.assertInstanceOf(Number.class, o2b);
        Assertions.assertEquals(1, o2b);
        final Object o3 = new JSONPointer("/gamma").queryFrom(jsonObject);
        Assertions.assertInstanceOf(JSONArray.class, o3);
        final Object o3a = new JSONPointer("/gamma/0").queryFrom(jsonObject);
        Assertions.assertInstanceOf(JSONObject.class, o3a);
        final Object o3aa = new JSONPointer("/gamma/0/g1").queryFrom(jsonObject);
        Assertions.assertInstanceOf(String.class, o3aa);
        Assertions.assertEquals("1", o3aa);
    }
}
