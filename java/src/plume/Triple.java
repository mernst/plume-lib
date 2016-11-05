package plume;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Mutable triple class: type-safely holds three objects of possibly-different types.
 *
 * @param <T1> the type of the first element of the triple
 * @param <T2> the type of the second element of the triple
 * @param <T3> the type of the third element of the triple
 */
public class Triple<T1, T2, T3> {
  public T1 a;
  public T2 b;
  public T3 c;

  public Triple(T1 a, T2 b, T3 c) {
    this.a = a;
    this.b = b;
    this.c = c;
  }

  /**
   * Factory method with short name and no need to name type parameters.
   *
   * @param <A> type of first argument
   * @param <B> type of second argument
   * @param <C> type of third argument
   * @param a first argument
   * @param b second argument
   * @param c third argument
   * @return a triple of the values (a, b, c)
   */
  public static <A, B, C> Triple<A, B, C> of(A a, B b, C c) {
    return new Triple<A, B, C>(a, b, c);
  }

  /*@SideEffectFree*/
  public String toString(/*>>>@GuardSatisfied Triple<T1,T2,T3> this*/) {
    return "<" + String.valueOf(a) + "," + String.valueOf(b) + "," + String.valueOf(c) + ">";
  }

  @SuppressWarnings("interning") // equality testing optimization
  /*@Pure*/
  public boolean equals(
      /*>>>@GuardSatisfied Triple<T1,T2,T3> this,*/
      /*@GuardSatisfied*/ /*@Nullable*/ Object obj) {
    if (obj instanceof Triple<?, ?, ?>) {
      @SuppressWarnings("unchecked")
      Triple<T1, T2, T3> other = (Triple<T1, T2, T3>) obj;
      boolean aEquals = ((this.a == other.a) || (this.a != null && (this.a.equals(other.a))));
      boolean bEquals = ((this.b == other.b) || (this.b != null && (this.b.equals(other.b))));
      boolean cEquals = ((this.c == other.c) || (this.c != null && (this.c.equals(other.c))));
      return aEquals && bEquals && cEquals;
    } else {
      return false;
    }
  }

  // See coment at Pair.hashCode.
  /*@Pure*/
  public int hashCode(/*>>>@GuardSatisfied Triple<T1,T2,T3> this*/) {
    return (((a == null) ? 0 : a.hashCode())
        + ((b == null) ? 0 : b.hashCode())
        + ((c == null) ? 0 : c.hashCode()));
  }
}
