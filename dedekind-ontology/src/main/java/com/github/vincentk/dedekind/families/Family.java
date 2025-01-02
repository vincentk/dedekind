package com.github.vincentk.dedekind.families;

import com.github.vincentk.dedekind.sets.Cardinality;
import com.github.vincentk.dedekind.sets.Element;
import com.github.vincentk.dedekind.sets.NonEmptySet;
import com.github.vincentk.dedekind.sets.Set;

/**
 * @see https://en.wikipedia.org/wiki/Indexed_family
 * @see https://en.wikipedia.org/wiki/Function_(mathematics)#Definition
 */
public interface Family<

//Domain and its elements:
De extends Element<De>,
C extends Cardinality,
D extends Set<De, C, D>,

//Range / Co-domain:
Re extends Element<Re>,

// Implementation details:
P extends Pair<De, Re, P>,
Z extends Family<De, C, D, Re, P, Z>
>
extends NonEmptySet<P, C, Z>
{
    /**
     * @return the image of the function.
     */
    <C1 extends C>
    Set<Re, C1, ?> image();
}
