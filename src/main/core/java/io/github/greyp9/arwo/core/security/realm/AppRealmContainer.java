package io.github.greyp9.arwo.core.security.realm;

public interface AppRealmContainer {
    String NAMING_CONTAINER = "javax.naming.Name-io.github.greyp9.arwo.core.security.AppRealmContainer";

    void setAppRealm(AppRealm appRealm);
}
