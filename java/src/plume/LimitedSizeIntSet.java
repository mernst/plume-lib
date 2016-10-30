package plume;

import java.io.Serializable;
import java.util.List;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * LimitedSizeIntSet stores up to some maximum number of unique integer values, at which point its
 * rep is nulled, in order to save space.
 *
 * <p>The advantage of this class over {@code LimitedSizeSet<Integer>} is that it does not autobox
 * the int values, so it takes less memory.
 *
 * @see LimitedSizeSet
 */
// Consider adding:
//  * @deprecated Use LimitedSizeSet instead
// @Deprecated
public class LimitedSizeIntSet implements Serializable, Cloneable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20031021L;

  // public final int max_values;

  /**
   * If null, then at least num_values distinct values have been seen. The size is not separately
   * stored, because that would take extra space.
   */
  protected int /*@Nullable*/ [] values;
  /** The number of active elements (equivalently, the first unused index). */
  int num_values;

  /**
   * Create a new LimitedSizeIntSet that can hold max_values values.
   *
   * @param max_values the maximum number of values this set will be able to hold
   */
  public LimitedSizeIntSet(int max_values) {
    assert max_values > 0;
    // this.max_values = max_values;
    values = new int[max_values];
    num_values = 0;
  }

  public void add(int elt) {
    if (values == null) {
      return;
    }

    if (contains(elt)) {
      return;
    }
    if (num_values == values.length) {
      values = null;
      num_values++;
      return;
    }
    values[num_values] = elt;
    num_values++;
  }

  public void addAll(LimitedSizeIntSet s) {
    @SuppressWarnings("interning") // optimization; not a subclass of Collection, though
    boolean sameObject = (this == s);
    if (sameObject) {
      return;
    }
    if (repNulled()) {
      return;
    }
    if (s.repNulled()) {
      int values_length = values.length;
      // We don't know whether the elements of this and the argument were
      // disjoint.  There might be anywhere from max(size(), s.size()) to
      // (size() + s.size()) elements in the resulting set.
      if (s.size() > values_length) {
        num_values = values_length + 1;
        values = null;
        return;
      } else {
        throw new Error(
            "Arg is rep-nulled, so we don't know its values and can't add them to this.");
      }
    }
    for (int i = 0; i < s.size(); i++) {
      assert s.values != null
          : "@AssumeAssertion(nullness): no relevant side effect:  add's side effects do not affect s.values";
      add(s.values[i]);
      if (repNulled()) {
        return; // optimization, not necessary for correctness
      }
    }
  }

  @SuppressWarnings("deterministic") // pure wrt equals() but not ==: throws a new exception
  /*@Pure*/
  public boolean contains(int elt) {
    if (values == null) {
      throw new UnsupportedOperationException();
    }
    for (int i = 0; i < num_values; i++) {
      if (values[i] == elt) {
        return true;
      }
    }
    return false;
  }

  /**
   * A lower bound on the number of elements in the set. Returns either the number of elements that
   * have been inserted in the set, or max_size(), whichever is less.
   *
   * @return a number that is a lower bound on the number of elements added to the set
   */
  /*@Pure*/
  public int size(/*>>>@GuardSatisfied LimitedSizeIntSet this*/) {
    return num_values;
  }

  /**
   * An upper bound on how many distinct elements can be individually represented in the set.
   * Returns max_values+1 (where max_values is the argument to the constructor).
   *
   * @return maximum capacity of the set representation
   */
  public int max_size() {
    if (values == null) {
      return num_values;
    } else {
      return values.length + 1;
    }
  }

  /*@EnsuresNonNullIf(result=false, expression="values")*/
  /*@Pure*/
  public boolean repNulled() {
    return values == null;
  }

  @SuppressWarnings("sideeffectfree") // side effect to local state (clone)
  /*@SideEffectFree*/
  public LimitedSizeIntSet clone(/*>>>@GuardSatisfied LimitedSizeIntSet this*/) {
    LimitedSizeIntSet result;
    try {
      result = (LimitedSizeIntSet) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
    if (values != null) {
      result.values = values.clone();
    }
    return result;
  }

  /**
   * Merges a list of LimitedSizeIntSet objects into a single object that represents the values seen
   * by the entire list. Returns the new object, whose max_values is the given integer.
   *
   * @param max_values the maximum size for the returned LimitedSizeIntSet
   * @param slist a list of LimitedSizeIntSet, whose elements will be merged
   * @return a LimitedSizeIntSet that merges the elements of slist
   */
  public static LimitedSizeIntSet merge(int max_values, List<LimitedSizeIntSet> slist) {
    LimitedSizeIntSet result = new LimitedSizeIntSet(max_values);
    for (LimitedSizeIntSet s : slist) {
      result.addAll(s);
    }
    return result;
  }

  /*@SideEffectFree*/
  public String toString(/*>>>@GuardSatisfied LimitedSizeIntSet this*/) {
    return ("[size="
        + size()
        + "; "
        + ((values == null) ? "null" : ArraysMDE.toString(values))
        + "]");
  }
}
