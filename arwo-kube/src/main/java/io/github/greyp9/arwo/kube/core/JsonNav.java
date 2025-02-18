package io.github.greyp9.arwo.kube.core;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.lang.MathU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class JsonNav {

    /**
     * Find the JsonElement referenced by the input path.  If the path is not found, an HTTP 404 response should be
     * returned.  If the last "jsonKey" is "-", the serialized JSON representation of the cursor element should be
     * returned.  Otherwise, the input "html" parameter should be populated with navigation links to the child
     * JsonElements.  This allows the user an easy means to navigate to a specific section of the input JSON.
     */
    public static HttpResponse toHttpResponse(final JsonElement jsonElement,
                                              final List<String> jsonKeys,
                                              final Element html) {
        return new JsonNav().toHttpResponseRecurse(jsonElement, jsonKeys, html);
    }


    private HttpResponse toHttpResponseRecurse(final JsonElement jsonElement,
                                               final List<String> jsonKeys,
                                               final Element html) {
        final HttpResponse httpResponse;
        if (jsonKeys.isEmpty()) {
            httpResponse = toHttpResponseAt(jsonElement, html);
        } else {
            final String jsonKey = jsonKeys.remove(0);
            httpResponse = toHttpResponse(jsonElement, jsonKey, jsonKeys, html);
        }
        return httpResponse;
    }

    private HttpResponse toHttpResponseAt(final JsonElement jsonElement, final Element html) {
        final HttpResponse httpResponse;
        if (jsonElement.isJsonObject()) {
            toHttpResponseAt(jsonElement.getAsJsonObject(), html);
            httpResponse = null;
        } else if (jsonElement.isJsonArray()) {
            toHttpResponseAt(jsonElement.getAsJsonArray(), html);
            httpResponse = null;
        } else if (jsonElement.isJsonPrimitive()) {
            httpResponse = toHttpResponseAt(jsonElement.getAsJsonPrimitive());
        } else {
            httpResponse = HttpResponseU.to500("UNKNOWN-TYPE");
        }
        return httpResponse;
    }

    private void toHttpResponseAt(final JsonObject jsonObject, final Element html) {
        addLink("[view json]", "-", html);
        // add links for child objects
        final Set<Map.Entry<String, JsonElement>> entries = jsonObject.entrySet();
        for (Map.Entry<String, JsonElement> entry : entries) {
            addLink(entry.getKey(), entry.getKey(), html);
        }
    }

    private void toHttpResponseAt(final JsonArray jsonArray, final Element html) {
        addLink("[view json]", "-", html);
        // add links for child array elements
        for (int i = 0; (i < jsonArray.size()); ++i) {
            final String index = Integer.toString(i);
            addLink(index, index, html);
        }
    }

    private HttpResponse toHttpResponseAt(final JsonPrimitive jsonPrimitive) {
        return toHttp200(UTF8Codec.toBytes(jsonPrimitive.toString()));
    }

    private HttpResponse toHttp200(final byte[] payload) {
        final long lastModified = new Date().getTime() / DurationU.Const.ONE_SECOND_MILLIS;
        final FileMetaData metaData = new FileMetaData(null, payload.length, lastModified, false);
        return HttpResponseU.to200(new MetaFile(metaData, Http.Mime.APP_JSON, new ByteArrayInputStream(payload)));
    }

    private void addLink(final String text, final String href, final Element html) {
        final Element div = ElementU.addElement(html, Html.DIV, null);
        ElementU.addElement(div, Html.A, text, NTV.create(Html.HREF, href + "/"));
    }

    private HttpResponse toHttpResponse(final JsonElement jsonElement,
                                        final String jsonKey, final List<String> jsonKeys,
                                        final Element html) {
        final HttpResponse httpResponse;
        if ("-".equals(jsonKey)) {
            httpResponse = toHttp200(UTF8Codec.toBytes(JsonU.toPretty(jsonElement)));
        } else if (jsonElement.isJsonObject()) {
            httpResponse = toHttpResponse(jsonElement.getAsJsonObject(), jsonKey, jsonKeys, html);
        } else if (jsonElement.isJsonArray()) {
            httpResponse = toHttpResponse(jsonElement.getAsJsonArray(), jsonKey, jsonKeys, html);
        } else {
            httpResponse = HttpResponseU.to400(jsonKey);
        }
        return httpResponse;
    }

    private HttpResponse toHttpResponse(final JsonObject jsonObject,
                                        final String jsonKey, final List<String> jsonKeys,
                                        final Element html) {
        return toHttpResponseRecurse(jsonObject.get(jsonKey), jsonKeys, html);
    }

    private HttpResponse toHttpResponse(final JsonArray jsonArray,
                                        final String jsonKey, final List<String> jsonKeys,
                                        final Element html) {
        final HttpResponse httpResponse;
        final int index = Value.parseInt(jsonKey, -1);
        if (MathU.isInBound(0, index, jsonArray.size() - 1)) {
            httpResponse = toHttpResponseRecurse(jsonArray.get(index), jsonKeys, html);
        } else {
            httpResponse = HttpResponseU.to400(jsonKey);
        }
        return httpResponse;
    }
}
