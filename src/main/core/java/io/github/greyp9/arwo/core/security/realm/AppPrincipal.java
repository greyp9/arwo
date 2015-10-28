package io.github.greyp9.arwo.core.security.realm;

import java.util.ArrayList;
import java.util.Collection;

public final class AppPrincipal implements java.security.Principal {
    private final String name;
    private final Collection<String> roles;

    public AppPrincipal(final String name, final Collection<String> roles) {
        this.name = name;
        this.roles = new ArrayList<String>();
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

    public Collection<String> getRoles() {
        return roles;
    }
}
