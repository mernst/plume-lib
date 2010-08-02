package plume;

/**
 * Mutable triple class:
 * type-safely holds three objects of possibly-different types.
 **/
public class Triple<T1,T2,T3> {
  public T1 a;
  public T2 b;
  public T3 c;

  public Triple(T1 a, T2 b, T3 c) {
    this.a = a;
    this.b = b;
    this.c = c;
  }

  /** Factory method with short name and no need to name type parameters. */
  public static <A, B, C> Triple<A, B, C> of(A a, B b, C c) {
    return new Triple<A, B, C>(a, b, c);
  }

  public String toString() {
    return "<" + String.valueOf(a)
      + "," + String.valueOf(b)
      + "," + String.valueOf(c)
      + ">";
  }

  @SuppressWarnings("interning")
  public boolean equals(/*@Nullable*/ Object obj) {
    if (obj instanceof Triple<?,?,?>) {
      @SuppressWarnings("unchecked")
      Triple<T1,T2,T3> other = (Triple<T1,T2,T3>) obj;
      boolean aEquals = ((this.a == other.a)
                         || (this.a != null && (this.a.equals(other.a))));
      boolean bEquals = ((this.b == other.b)
                         || (this.b != null && (this.b.equals(other.b))));
      boolean cEquals = ((this.c == other.c)
                         || (this.c != null && (this.c.equals(other.c))));
      return aEquals && bEquals && cEquals;
    } else {
      return false;
    }
  }

  // See coment at Pair.hashCode.
  public int hashCode() {
    return (((a == null) ? 0 : a.hashCode())
            + ((b == null) ? 0 : b.hashCode())
            + ((c == null) ? 0 : c.hashCode()));
  }

}
