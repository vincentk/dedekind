/**
 * 
 */
package com.github.vincentk.dedekind.linear.matrix;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import static com.github.vincentk.dedekind.linear.finite.One.one;
import static com.github.vincentk.dedekind.numbers.B.bool;

import static com.github.vincentk.dedekind.linear.vector.arrays.Booleans.booleans;

public class OfVectorTest {
    
    @Test
    public void testRowMatrixBoolean() {
        // x = {{false, true}}
        final var X = new OfVector.Row<>(booleans(false, true));
        
        // X2 = {{false, true}} {{false, true}}' = {{true}}
        final var X2 = X.compose(X.transpose());
        
        // X2d = X2 + X2 = {{true}} + {{true}} = {{true}}
        final var X2d = X2.plus(X2);
        
        // {{true}} {true} = {true}
        final var Y = X2d.apply(one(bool(true)));
        
        assertThat(Y.val()).isEqualTo(bool(true));
    }
    
    @Test
    public void testColMatrixBoolean() {
        
        // x = {false, true}
        final var x = booleans(false, true);
        
        // x' = {false, true}'
        final var xt = x.transpose();
        
        // X = {{false, true}}'
        final var X = new OfVector.Column<>(xt);
        
        // X2 = {{false, true}}' {{false, true}} = {{true}}
        final var X2 = X.compose(X.transpose());
        
        // Y = {false, true}' = x
        final var Y = X2.apply(xt);
        
        assertThat(Y.val()).isEqualTo(x);
    }
}
