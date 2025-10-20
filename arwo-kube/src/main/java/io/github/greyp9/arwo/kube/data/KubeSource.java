package io.github.greyp9.arwo.kube.data;

public final class KubeSource {

    private KubeSource() {
    }

    // kube context table views
    static final String FIELD_SELECT = "select";
    static final String FIELD_CREATED = "created";
    static final String FIELD_CPU = "cpu";
    static final String FIELD_DATA = "data";
    static final String FIELD_FINALIZERS = "finalizers";
    static final String FIELD_HOST_IP = "hostIP";
    static final String FIELD_IMAGE = "image";
    static final String FIELD_INIT = "init";
    static final String FIELD_MEMORY = "memory";
    static final String FIELD_NAME = "name";
    static final String FIELD_NAMESPACE = "namespace";
    static final String FIELD_POD_IP = "podIP";
    static final String FIELD_PORTS = "ports";
    static final String FIELD_READY = "ready";
    static final String FIELD_RESTARTS = "restarts";
    static final String FIELD_STATE = "state";
    static final String FIELD_STATUS = "status";
    static final String FIELD_STORAGE = "storage";
    static final String FIELD_TAINTS = "taints";
    static final String FIELD_TYPE = "type";
    static final String FIELD_VERSION = "version";

    static final String STATUS_CPU = "cpu";
    static final String STATUS_MEMORY = "memory";
    static final String STATUS_STORAGE = "ephemeral-storage";

    static final String CONTEXT_CONTAINERS = "containers";
    static final String CONTEXT_DESCRIBE = "describe";
    static final String CONTEXT_LOGS = "logs";
    static final String CONTEXT_NODES = "nodes";
    static final String CONTEXT_PODS = "pods";
    static final String CONTEXT_SECRETS = "secrets";
}
