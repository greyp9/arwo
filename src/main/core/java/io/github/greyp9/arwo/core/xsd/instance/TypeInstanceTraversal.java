package io.github.greyp9.arwo.core.xsd.instance;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class TypeInstanceTraversal {
    private final List<List<TypeInstance>> traversal;

    public TypeInstanceTraversal(final TypeInstance typeInstance) {
        this.traversal = new ArrayList<List<TypeInstance>>();
        traverse(typeInstance, new ArrayList<TypeInstance>());
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void traverse(final TypeInstance typeInstance, final List<TypeInstance> traversalIt) {
        final Collection<TypeInstance> typeInstancesIt = typeInstance.getInstances();
        for (final TypeInstance typeInstanceIt : typeInstancesIt) {
            final List<TypeInstance> traversalIt1 = new ArrayList<TypeInstance>(traversalIt);
            if (typeInstanceIt instanceof ChoiceTypeInstance) {
                traversalIt1.add(typeInstanceIt);
                traverse(typeInstanceIt, traversalIt1);
            } else {
                traversalIt1.add(typeInstanceIt);
                traversal.add(traversalIt1);
            }
        }
    }

    public final List<TypeInstance> getForName(final String name) {
        List<TypeInstance> forNameInstances = new ArrayList<TypeInstance>();
        for (final List<TypeInstance> typeInstances : traversal) {
            // find endpoint
            String nameIt = null;
            for (final TypeInstance typeInstance : typeInstances) {
                nameIt = typeInstance.getName();
            }
            // compare endpoint name to what is sought
            if (name.equals(nameIt)) {
                forNameInstances = typeInstances;
                break;
            }
        }
        return forNameInstances;
    }
}
