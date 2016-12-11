package plume;

import java.io.Serializable;
import java.util.List;

/*>>>
import org.checkerframework.checker.index.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.lowerbound.qual.*;
import org.checkerframework.checker.minlen.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * LimitedSizeSet stores up to some maximum number of unique values, at which point its rep is
 * nulled, in order to save space.
 *
 * @param <T> the type of elements in the set
 */
public class LimitedSizeSet<T> implements Serializable, Cloneable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20031021L;

  // public final int max_values;

  /**
   * If null, then at least num_values distinct values have been seen. The size is not separately
   * stored, because that would take extra space.
   */
  protected /*@Nullable*/ T /*@Nullable*/ /*@MinLen(1)*/ [] values;
  /** The number of active elements (equivalently, the first unused index). */
  // Not exactly @IndexOrHigh("values"), because the values field can get
  // nulled.  But that should be permitted by the type system.
  /*@IndexOrHigh("values")*/ int num_values;

  /**
   * Create a new LimitedSizeSet that can hold max_values values.
   *
   * @param max_values the maximum number of values this set will be able to hold
   */
  public LimitedSizeSet(/*@Positive*/ int max_values) {
    assert max_values > 0;
    // this.max_values = max_values;
    @SuppressWarnings("unchecked")
    /*@Nullable*/ T[] new_values_array = (/*@Nullable*/ T[]) new /*@Nullable*/ Object[max_values];
    values = new_values_array;
    num_values = 0;
  }

  @SuppressWarnings("index") // num_values may or may not be an index
  public void add(T elt) {
    if (repNulled()) {
      return;
    }

    if (contains(elt)) {
      return;
    }
    if (num_values == values.length) {
      nullRep();
      return;
    }
    values[num_values] = elt;
    num_values++;
  }

  public void addAll(LimitedSizeSet<? extends T> s) {
    @SuppressWarnings("interning") // optimization; not a subclass of Collection, though
    boolean sameObject = (this == s);
    if (sameObject) {
      return;
    }
    if (repNulled()) {
      return;
    }
    if (s.repNulled()) {
      // We don't know whether the elements of this and the argument were
      // disjoint.  There might be anywhere from max(size(), s.size()) to
      // (size() + s.size()) elements in the resulting set.
      if (s.size() > values.length) {
        nullRep();
        return;
      } else {
        throw new Error(
            "Arg is rep-nulled, so we don't know its values and can't add them to this.");
      }
    }
    // s.values isn't modified by the call to add.  Until
    // https://github.com/typetools/checker-framework/issues/984 is fixed,
    // use a local variable which the Checker Framework can tell is not reassigned.
    T[] svalues = s.values;
    for (int i = 0; i < s.size(); i++) {
      add(svalues[i]);
      if (repNulled()) {
        return; // optimization, not necessary for correctness
      }
    }
  }

  @SuppressWarnings({
    "deterministic", // pure wrt equals() but not ==: throws a new exception
    "index" // num_values may or may not be an index
  })
  /*@Pure*/
  public boolean contains(T elt) {
    if (repNulled()) {
      throw new UnsupportedOperationException();
    }
    for (int i = 0; i < num_values; i++) {
      @SuppressWarnings("nullness") // object invariant: used portion of array
      T value = values[i];
      if (value == elt || (value != null && value.equals(elt))) {
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
  public int size(/*>>>@GuardSatisfied LimitedSizeSet<T> this*/) {
    return num_values;
  }

  /**
   * An upper bound on how many distinct elements can be individually represented in the set.
   * Returns max_values+1 (where max_values is the argument to the constructor).
   *
   * @return maximum capacity of the set representation
   */
  public /*@Positive*/ int max_size() {
    if (repNulled()) {
      return num_values;
    } else {
      return values.length + 1;
    }
  }

  /**
   * Returns true if more elements have been added than this set can contain (which is the integer
   * that was passed to the constructor when creating this set).
   */
  /*@EnsuresNonNullIf(result=false, expression="values")*/
  /*@Pure*/
  public boolean repNulled() {
    return values == null;
  }

  /**
   * Null the representation, which happens when a client tries to add more elements to this set
   * than it can contain (which is the integer that was passed to the constructor when creating this
   * set).
   */
  private void nullRep() {
    num_values = values.length + 1;
    values = null;
  }

  @SuppressWarnings("sideeffectfree") // side effect to local state (clone)
  /*@SideEffectFree*/
  public LimitedSizeSet<T> clone(/*>>>@GuardSatisfied LimitedSizeSet<T> this*/) {
    LimitedSizeSet<T> result;
    try {
      @SuppressWarnings("unchecked")
      LimitedSizeSet<T> result_as_lss = (LimitedSizeSet<T>) super.clone();
      result = result_as_lss;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
    if (values != null) {
      result.values = values.clone();
    }
    return result;
  }

  /**
   * Merges a list of {@code LimitedSizeSet<T>} objects into a single object that represents the
   * values seen by the entire list. Returns the new object, whose max_values is the given integer.
   *
   * @param <T> (super)type of elements of the sets
   * @param max_values the maximum size for the returned LimitedSizeSet
   * @param slist a list of LimitedSizeSet, whose elements will be merged
   * @return a LimitedSizeSet that merges the elements of slist
   */
  public static <T> LimitedSizeSet<T> merge(
      /*@Positive*/ int max_values, List<LimitedSizeSet<? extends T>> slist) {
    LimitedSizeSet<T> result = new LimitedSizeSet<T>(max_values);
    for (LimitedSizeSet<? extends T> s : slist) {
      result.addAll(s);
    }
    return result;
  }

  @SuppressWarnings("nullness") // bug in flow; to fix later
  /*@SideEffectFree*/
  public String toString(/*>>>@GuardSatisfied LimitedSizeSet<T> this*/) {
    return ("[size=" + size() + "; " + ((repNulled()) ? "null" : ArraysMDE.toString(values)) + "]");
  }
}
