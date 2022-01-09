package io.github.greyp9.arwo.core.security.realm;

import java.util.ArrayList;
import java.util.Collection;

public final class AppPrincipal implements java.security.Principal {
    private final String name;
    private final boolean authenticated;
    private final Collection<String> roles;

    public AppPrincipal(final String name, final Collection<String> roles) {
        this(name, true, roles);
    }

    public AppPrincipal(final String name, final boolean authenticated, final Collection<String> roles) {
        this.name = name;
        this.authenticated = authenticated;
        this.roles = new ArrayList<>();
        if (roles != null) {
            this.roles.addAll(roles);
        }
    }

    public String getName() {
        return name;
    }

    public String toString() {
        return String.format("[%s][%s][%s]", getClass().getName(), name, roles);
    }

    public boolean isAuthenticated() {
        return authenticated;
    }

    public Collection<String> getRoles() {
        return roles;
    }
}
