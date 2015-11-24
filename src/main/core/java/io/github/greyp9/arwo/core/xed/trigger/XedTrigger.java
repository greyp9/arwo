package io.github.greyp9.arwo.core.xed.trigger;

import io.github.greyp9.arwo.core.xed.model.Xed;

public interface XedTrigger {
    void onPersist(String contextPath, Xed xed);
}
