// If you edit this file, you must also edit its tests.
// For tests of this and the entire plume package, see class TestPlume.

package plume;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.Vector;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
import org.checkerframework.framework.qual.PolyAll;
*/

/**
 * Utilities for manipulating arrays and collections. This complements {@link java.util.Arrays} and
 * {@link java.util.Collections}.
 */
@SuppressWarnings("interning") // to do later
public final class ArraysMDE {
  /** This class is a collecton of methods; it does not represent anything. */
  private ArraysMDE() {
    throw new Error("do not instantiate");
  }

  ///////////////////////////////////////////////////////////////////////////
  /// min, max
  ///

  // Could also add linear-time orderStatistics if I liked.

  /**
   * Return the smallest value in the array.
   *
   * @param a an array
   * @return the smallest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static int min(int[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min(int[])");
    }
    int result = a[0];
    for (int i = 1; i < a.length; i++) {
      result = Math.min(result, a[i]);
    }
    return result;
  }

  /**
   * Return the smallest value in the array.
   *
   * @param a an array
   * @return the smallest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static long min(long[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min(long[])");
    }
    long result = a[0];
    for (int i = 1; i < a.length; i++) {
      result = Math.min(result, a[i]);
    }
    return result;
  }

  /**
   * Return the smallest value in the array.
   *
   * @param a an array
   * @return the smallest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static double min(double[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min(double[])");
    }
    double result = a[0];
    for (int i = 1; i < a.length; i++) {
      result = Math.min(result, a[i]);
    }
    return result;
  }

  /**
   * Return the smallest value in the array.
   *
   * @param a an array
   * @return the smallest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static Integer min(Integer[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min(Integer[])");
    }
    Integer result = a[0]; // to return a value actually in the array
    int result_int = result.intValue(); // for faster comparison
    for (int i = 1; i < a.length; i++) {
      if (a[i].intValue() < result_int) {
        result = a[i];
        result_int = result.intValue();
      }
    }
    return result;
  }

  /**
   * Return the smallest value in the array.
   *
   * @param a an array
   * @return the smallest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static Long min(Long[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min(Long[])");
    }
    Long result = a[0]; // to return a value actually in the array
    long result_long = result.longValue(); // for faster comparison
    for (int i = 1; i < a.length; i++) {
      if (a[i].longValue() < result_long) {
        result = a[i];
        result_long = result.longValue();
      }
    }
    return result;
  }

  /**
   * Return the smallest value in the array.
   *
   * @param a an array
   * @return the smallest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static Double min(Double[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min(Double[])");
    }
    Double result = a[0]; // to return a value actually in the array
    int result_int = result.intValue(); // for faster comparison
    for (int i = 1; i < a.length; i++) {
      if (a[i].intValue() < result_int) {
        result = a[i];
        result_int = result.intValue();
      }
    }
    return result;
  }

  /**
   * Return the largest value in the array.
   *
   * @param a an array
   * @return the largest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static int max(int[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(int[])");
    }
    int result = a[0];
    for (int i = 1; i < a.length; i++) {
      result = Math.max(result, a[i]);
    }
    return result;
  }

  /**
   * Return the largest value in the array.
   *
   * @param a an array
   * @return the largest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static long max(long[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(long[])");
    }
    long result = a[0];
    for (int i = 1; i < a.length; i++) {
      result = Math.max(result, a[i]);
    }
    return result;
  }

  /**
   * Return the largest value in the array.
   *
   * @param a an array
   * @return the largest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static double max(double[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(double[])");
    }
    double result = a[0];
    for (int i = 1; i < a.length; i++) {
      result = Math.max(result, a[i]);
    }
    return result;
  }

  /**
   * Return the largest value in the array.
   *
   * @param a an array
   * @return the largest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static Integer max(Integer[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(Integer[])");
    }
    Integer result = a[0]; // to return a value actually in the array
    int result_int = result.intValue(); // for faster comparison
    for (int i = 1; i < a.length; i++) {
      if (a[i].intValue() > result_int) {
        result = a[i];
        result_int = result.intValue();
      }
    }
    return result;
  }

  /**
   * Return the largest value in the array.
   *
   * @param a an array
   * @return the largest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static Long max(Long[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(Long[])");
    }
    Long result = a[0]; // to return a value actually in the array
    long result_long = result.longValue(); // for faster comparison
    for (int i = 1; i < a.length; i++) {
      if (a[i].longValue() > result_long) {
        result = a[i];
        result_long = result.longValue();
      }
    }
    return result;
  }

  /**
   * Return the largest value in the array.
   *
   * @param a an array
   * @return the largest value in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static Double max(Double[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(Double[])");
    }
    Double result = a[0]; // to return a value actually in the array
    int result_int = result.intValue(); // for faster comparison
    for (int i = 1; i < a.length; i++) {
      if (a[i].intValue() > result_int) {
        result = a[i];
        result_int = result.intValue();
      }
    }
    return result;
  }

  /**
   * Return a two-element array containing the smallest and largest values in the array.
   *
   * @param a an array
   * @return a two-element array containing the smallest and largest values in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static int[] min_max(int[] a) {
    if (a.length == 0) {
      // return null;
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min_max(int[])");
    }
    int result_min = a[0];
    int result_max = a[0];
    for (int i = 1; i < a.length; i++) {
      result_min = Math.min(result_min, a[i]);
      result_max = Math.max(result_max, a[i]);
    }
    return new int[] {result_min, result_max};
  }

  /**
   * Return a two-element array containing the smallest and largest values in the array.
   *
   * @param a an array
   * @return a two-element array containing the smallest and largest values in the array
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static long[] min_max(long[] a) {
    if (a.length == 0) {
      // return null;
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min_max(long[])");
    }
    long result_min = a[0];
    long result_max = a[0];
    for (int i = 1; i < a.length; i++) {
      result_min = Math.min(result_min, a[i]);
      result_max = Math.max(result_max, a[i]);
    }
    return new long[] {result_min, result_max};
  }

  /**
   * Return the difference between the smallest and largest array elements.
   *
   * @param a an array
   * @return the difference between the smallest and largest array elements
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static int element_range(int[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to element_range(int[])");
    }
    int[] min_max = min_max(a);
    return min_max[1] - min_max[0];
  }

  /**
   * Return the difference between the smallest and largest array elements.
   *
   * @param a an array
   * @return the difference between the smallest and largest array elements
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   */
  public static long element_range(long[] a) {
    if (a.length == 0) {
      throw new ArrayIndexOutOfBoundsException("Empty array passed to element_range(long[])");
    }
    long[] min_max = min_max(a);
    return min_max[1] - min_max[0];
  }

  /**
   * Returns the sum of an array of integers.
   *
   * @param a an array
   * @return the sum of an array of integers
   */
  public static int sum(int[] a) {
    int sum = 0;
    for (int i = 0; i < a.length; i++) {
      sum += a[i];
    }
    return sum;
  }

  /**
   * Returns the sum of all the elements of a 2d array of integers.
   *
   * @param a a 2d array
   * @return the sum of all the elements of a 2d array of integers
   */
  public static int sum(int[][] a) {
    int sum = 0;
    for (int i = 0; i < a.length; i++) {
      for (int j = 0; j < a[i].length; j++) {
        sum += a[i][j];
      }
    }
    return sum;
  }

  /**
   * Returns the sum of an array of doubles.
   *
   * @param a an array
   * @return the sum of an array of doubles
   */
  public static double sum(double[] a) {
    double sum = 0;
    for (int i = 0; i < a.length; i++) {
      sum += a[i];
    }
    return sum;
  }

  /**
   * Returns the sum of all the elements of a 2d array of doubles.
   *
   * @param a a 2d array
   * @return the sum of all the elements of a 2d array of doubles
   */
  public static double sum(double[][] a) {
    double sum = 0;
    for (int i = 0; i < a.length; i++) {
      for (int j = 0; j < a[i].length; j++) {
        sum += a[i][j];
      }
    }
    return sum;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// indexOf
  ///

  /**
   * Searches for the first occurrence of the given element in the array, testing for equality using
   * the equals method.
   *
   * @param <T> type of the elements of the array
   * @param a an array
   * @param elt the element to search for
   * @return the first index whose element is equal to the specified element, or -1 if no such
   *     element is found in the array
   * @see java.util.List#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static <T> int indexOf(T[] a, /*@Nullable*/ Object elt) {
    if (elt == null) {
      return indexOfEq(a, elt);
    }
    for (int i = 0; i < a.length; i++) {
      if (elt.equals(a[i])) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array, testing for equality using
   * the equals method.
   *
   * @param <T> the type of the elements
   * @param a an array
   * @param elt the element to search for
   * @param minindex first index at which to search
   * @param indexlimit first index at which not to search
   * @return the first index i containing the specified element, such that <code>
   *     minindex &le; i &lt; indexlimit</code>, or -1 if the element is not found in that section
   *     of the array
   * @see java.util.List#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static <T> int indexOf(T[] a, /*@Nullable*/ Object elt, int minindex, int indexlimit) {
    if (elt == null) {
      return indexOfEq(a, elt, minindex, indexlimit);
    }
    for (int i = minindex; i < indexlimit; i++) {
      if (elt.equals(a[i])) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the list, testing for equality using
   * the equals method. Identical to List.indexOf, but included for completeness.
   *
   * @param a a list
   * @param elt the element to search for
   * @return the first index whose element is equal to the specified element, or -1 if no such
   *     element is found in the list
   * @see java.util.List#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(List<? extends /*@PolyNull*/ Object> a, /*@Nullable*/ Object elt) {
    return a.indexOf(elt);
  }

  /**
   * Searches for the first occurrence of the given element in the list, testing for equality using
   * the equals method.
   *
   * @param a a list
   * @param elt the element to search for
   * @param minindex first index at which to search
   * @param indexlimit first index at which not to search
   * @return the first index i containing the specified element, such that <code>
   *     minindex &le; i &lt; indexlimit</code>, or -1 if the element is not found in that section
   *     of the list
   * @see java.util.List#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(
      List<? extends /*@PolyNull*/ Object> a,
      /*@Nullable*/ Object elt,
      int minindex,
      int indexlimit) {
    if (elt == null) {
      return indexOfEq(a, elt, minindex, indexlimit);
    }
    for (int i = minindex; i < indexlimit; i++) {
      if (elt.equals(a.get(i))) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array, testing for equality using
   * == (not the equals method).
   *
   * @param a an array
   * @param elt the element to search for
   * @return the first index containing the specified element, or -1 if the element is not found in
   *     the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOfEq(/*@PolyAll*/ Object[] a, /*@Nullable*/ Object elt) {
    for (int i = 0; i < a.length; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array, testing for equality using
   * == (not the equals method).
   *
   * @param a an array
   * @param elt the element to search for
   * @param minindex first index at which to search
   * @param indexlimit first index at which not to search
   * @return the first index i containing the specified element, such that <code>
   *     minindex &le; i &lt; indexlimit</code>, or -1 if the element is not found in that section
   *     of the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOfEq(
      /*@PolyNull*/ Object[] a, /*@Nullable*/ Object elt, int minindex, int indexlimit) {
    for (int i = minindex; i < indexlimit; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the list, testing for equality using
   * == (not the equals method).
   *
   * @param a a list
   * @param elt the element to search for
   * @return the first index containing the specified element, or -1 if the element is not found in
   *     the list
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOfEq(List<? extends /*@PolyNull*/ Object> a, /*@Nullable*/ Object elt) {
    for (int i = 0; i < a.size(); i++) {
      if (elt == a.get(i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the list, testing for equality using
   * == (not the equals method).
   *
   * @param a a list
   * @param elt the element to search for
   * @param minindex first index at which to search
   * @param indexlimit first index at which not to search
   * @return the first index i containing the specified element, such that <code>
   *     minindex &le; i &lt; indexlimit</code>, or -1 if the element is not found in that section
   *     of the list
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOfEq(
      List<? extends /*@PolyNull*/ Object> a,
      /*@Nullable*/ Object elt,
      int minindex,
      int indexlimit) {
    for (int i = minindex; i < indexlimit; i++) {
      if (elt == a.get(i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array.
   *
   * @param a an array
   * @param elt the element to search for
   * @return the first index containing the specified element, or -1 if the element is not found in
   *     the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(int[] a, int elt) {
    for (int i = 0; i < a.length; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array.
   *
   * @param a an array
   * @param elt the element to search for
   * @return the first index containing the specified element, or -1 if the element is not found in
   *     the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(long[] a, long elt) {
    for (int i = 0; i < a.length; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array.
   *
   * @param a an array
   * @param elt the element to search for
   * @param minindex first index at which to search
   * @param indexlimit first index at which not to search
   * @return the first index i containing the specified element, such that <code>
   *     minindex &le; i &lt; indexlimit</code>, or -1 if the element is not found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(int[] a, int elt, int minindex, int indexlimit) {
    for (int i = minindex; i < indexlimit; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array.
   *
   * @param a an array
   * @param elt the element to search for
   * @param minindex first index at which to search
   * @param indexlimit first index at which not to search
   * @return the first index i containing the specified element, such that <code>
   *     minindex &le; i &lt; indexlimit</code>, or -1 if the element is not found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(long[] a, long elt, int minindex, int indexlimit) {
    for (int i = minindex; i < indexlimit; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array.
   *
   * @param a an array
   * @param elt the element to search for
   * @return the first index containing the specified element, or -1 if the element is not found in
   *     the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(boolean[] a, boolean elt) {
    for (int i = 0; i < a.length; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array.
   *
   * @param a an array
   * @param elt the element to search for
   * @return the first index containing the specified element, or -1 if the element is not found in
   *     the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(double[] a, double elt) {
    for (int i = 0; i < a.length; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first occurrence of the given element in the array.
   *
   * @param a an array
   * @param elt the element to search for
   * @param minindex first index at which to search
   * @param indexlimit first index at which not to search
   * @return the first index i containing the specified element, such that <code>
   *     minindex &le; i &lt; indexlimit</code>, or -1 if the element is not found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   */
  /*@Pure*/
  public static int indexOf(boolean[] a, boolean elt, int minindex, int indexlimit) {
    for (int i = minindex; i < indexlimit; i++) {
      if (elt == a[i]) {
        return i;
      }
    }
    return -1;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// indexOf, for finding subarrays
  ///

  // This is analogous to Common Lisp's "search" function.

  // This implementation is very inefficient; I could use tricky Boyer-Moore
  // search techniques if I liked, but it's not worth it to me yet.

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise,
   * testing for equality using the equals method.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @return the first index at which the second array starts in the first array, or -1 if no such
   *     element is found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOf(/*@PolyAll*/ Object[] a, Object[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarray(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise,
   * testing for equality using == (not the equals method).
   *
   * @param a an array
   * @param sub subsequence to search for
   * @return the first index at which the second array starts in the first array, or -1 if the
   *     element is not found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  // The signature on this method is unnecessarily strict because it
  // requires that the component types be identical.  The signature should
  // be indexOfEq(@PolyAll(1) Object[], @PolyAll(2) Object[]), but the
  // @PolyAll qualifier does not yet take an argument.
  /*@Pure*/
  public static int indexOfEq(/*@PolyAll*/ Object[] a, /*@PolyAll*/ Object[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarrayEq(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the list that matches the given array elementwise,
   * testing for equality using the equals method.
   *
   * @param a a list
   * @param sub subsequence to search for
   * @return the first index at which the second array starts in the first list, or -1 if no such
   *     element is found in the list
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOf(List<?> a, /*@PolyAll*/ Object[] sub) {
    int a_index_max = a.size() - sub.length + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarray(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the list that matches the given array elementwise,
   * testing for equality using == (not the equals method).
   *
   * @param a a list
   * @param sub subsequence to search for
   * @return the first index at which the second array starts in the first list, or -1 if the
   *     element is not found in the list
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOfEq(List<?> a, /*@PolyAll*/ Object[] sub) {
    int a_index_max = a.size() - sub.length + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarrayEq(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given list elementwise,
   * testing for equality using the equals method.
   *
   * @param a a list
   * @param sub subsequence to search for
   * @return the first index at which the second list starts in the first array, or -1 if no such
   *     element is found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOf(/*@PolyAll*/ Object[] a, List<?> sub) {
    int a_index_max = a.length - sub.size() + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarray(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given list elementwise,
   * testing for equality using == (not the equals method).
   *
   * @param a a list
   * @param sub subsequence to search for
   * @return the first index at which the second list starts in the first array, or -1 if the
   *     element is not found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOfEq(/*@PolyAll*/ Object[] a, List<?> sub) {
    int a_index_max = a.length - sub.size() + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarrayEq(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the list that matches the given list elementwise, testing
   * for equality using the equals method.
   *
   * @param a a list
   * @param sub subsequence to search for
   * @return the first index at which the second list starts in the first list, or -1 if no such
   *     element is found in the list
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOf(List<?> a, List<?> sub) {
    int a_index_max = a.size() - sub.size() + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarray(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the list that matches the given list elementwise, testing
   * for equality using == (not the equals method).
   *
   * @param a a list
   * @param sub subsequence to search for
   * @return the first index at which the second list starts in the first list, or -1 if the element
   *     is not found in the list
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOfEq(List<?> a, List<?> sub) {
    int a_index_max = a.size() - sub.size() + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarrayEq(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @return the first index at which the second array starts in the first array, or -1 if the
   *     element is not found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOf(int[] a, int[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarray(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @return the first index at which the second array starts in the first array, or -1 if the
   *     element is not found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOf(double[] a, double[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarray(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @return the first index at which the second array starts in the first array, or -1 if the
   *     element is not found in the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOf(long[] a, long[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarray(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @return the first index containing the specified element, or -1 if the element is not found in
   *     the array
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   */
  /*@Pure*/
  public static int indexOf(boolean[] a, boolean[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i = 0; i <= a_index_max; i++) {
      if (isSubarray(a, sub, i)) {
        return i;
      }
    }
    return -1;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// mismatch
  ///

  // This is analogous to Common Lisp's "mismatch" function.

  // Put it off until later; for now, use the simpler subarray function,
  // which is a specialization of mismatch,

  ///////////////////////////////////////////////////////////////////////////
  /// subarray extraction
  ///

  // Note that the second argument is a length, not an end position.
  // That's to avoid confusion over whether it would be the last included
  // index or the first non-included index.

  /**
   * Return a subarray of the given array.
   *
   * @return a subarray of the given array
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   */
  /*@SideEffectFree*/
  public static /*@PolyAll*/ Object[] subarray(
      /*@PolyAll*/ Object[] a, int startindex, int length) {
    /*@PolyAll*/ Object[] result = new /*@PolyAll*/ Object[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a sublist of the given list.
   *
   * @param <T> type of the elements
   * @param a the original list
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a sublist of the given list
   */
  /*@SideEffectFree*/
  public static <T> List<T> subarray(List<T> a, int startindex, int length) {
    return a.subList(startindex, startindex + length);
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static /*@PolyAll*/ String[] subarray(
      /*@PolyAll*/ String[] a, int startindex, int length) {
    /*@PolyAll*/ String[] result = new /*@PolyAll*/ String[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static byte[] subarray(byte[] a, int startindex, int length) {
    byte[] result = new byte[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static boolean[] subarray(boolean[] a, int startindex, int length) {
    boolean[] result = new boolean[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static char[] subarray(char[] a, int startindex, int length) {
    char[] result = new char[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static double[] subarray(double[] a, int startindex, int length) {
    double[] result = new double[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static float[] subarray(float[] a, int startindex, int length) {
    float[] result = new float[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static int[] subarray(int[] a, int startindex, int length) {
    int[] result = new int[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static long[] subarray(long[] a, int startindex, int length) {
    long[] result = new long[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  /**
   * Return a subarray of the given array.
   *
   * @param a the original array
   * @param startindex the first index to be included
   * @param length the number of elements to include (not an end index, to avoid confusion over
   *     whether it would be the last included index or the first non-included index)
   * @return a subarray of the given array
   */
  /*@SideEffectFree*/
  public static short[] subarray(short[] a, int startindex, int length) {
    short[] result = new short[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// subarray testing
  ///

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first, testing for equality using the equals method.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarray(
      /*@PolyAll*/ Object[] a, /*@PolyNull*/ Object[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (!Objects.equals(sub[i], a[a_offset + i])) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first, testing for equality using == (not the equals method).
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarrayEq(
      /*@PolyAll*/ Object[] a, /*@PolyAll*/ Object[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (sub[i] != a[a_offset + i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first, testing for equality using the equals method.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return the first index at which the second array starts in the first array, or -1 if no such
   *     element is found in the array
   */
  /*@Pure*/
  public static boolean isSubarray(/*@PolyAll*/ Object[] a, List<?> sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.size();
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (!Objects.equals(sub.get(i), a[a_offset + i])) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first, testing for equality using == (not the equals method).
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarrayEq(/*@PolyAll*/ Object[] a, List<?> sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.size();
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (sub.get(i) != a[a_offset + i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first, testing for equality using the equals method.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return the first index at which the second array starts in the first array, or -1 if no such
   *     element is found in the array
   */
  /*@Pure*/
  public static boolean isSubarray(List<?> a, /*@PolyAll*/ Object[] sub, int a_offset) {
    int a_len = a.size() - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (!Objects.equals(sub[i], a.get(a_offset + i))) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first, testing for equality using == (not the equals method).
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarrayEq(List<?> a, /*@PolyAll*/ Object[] sub, int a_offset) {
    int a_len = a.size() - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (sub[i] != a.get(a_offset + i)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first, testing for equality using the equals method.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return the first index at which the second array starts in the first array, or -1 if no such
   *     element is found in the array
   */
  /*@Pure*/
  public static boolean isSubarray(List<?> a, List<?> sub, int a_offset) {
    int a_len = a.size() - a_offset;
    int sub_len = sub.size();
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (!Objects.equals(sub.get(i), a.get(a_offset + i))) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first, testing for equality using == (not the equals method).
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarrayEq(List<?> a, List<?> sub, int a_offset) {
    int a_len = a.size() - a_offset;
    int sub_len = sub.size();
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (sub.get(i) != a.get(a_offset + i)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarray(int[] a, int[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (sub[i] != a[a_offset + i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarray(long[] a, long[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (sub[i] != a[a_offset + i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarray(double[] a, double[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (sub[i] != a[a_offset + i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first, starting at the specified index
   * of the first.
   *
   * @param a an array
   * @param sub subsequence to search for
   * @param a_offset first index at which to search
   * @return true iff sub is a contiguous subarray of a
   */
  /*@Pure*/
  public static boolean isSubarray(boolean[] a, boolean[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len) {
      return false;
    }
    for (int i = 0; i < sub_len; i++) {
      if (sub[i] != a[a_offset + i]) {
        return false;
      }
    }
    return true;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// concatenation
  ///

  // Concat used to return null if both arguments are null.  That is
  // convenient for the implementer, but not so good for clients.

  /**
   * Call this method in order to suppress compiler warnings.
   *
   * @param <T> the base type of the result
   * @param lst the list to convert to an array
   * @return the result of lst.toArray, casted to a more precise type than Object[]
   */
  private static <T> T[] toTArray(List<T> lst) {
    @SuppressWarnings("unchecked")
    T[] asArray = (T[]) lst.toArray();
    return asArray;
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param <T> the type of the sequence elements
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static <T> T[] concat(T /*@Nullable*/ [] a, T /*@Nullable*/ [] b) {
    if (a == null) {
      if (b != null) {
        return b;
      } else {
        @SuppressWarnings("unchecked")
        T[] result = (T[]) new Object[0];
        return result;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        @SuppressWarnings("unchecked")
        T[] result = (T[]) new /*@MonotonicNonNull*/ Object[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param <T> the type of the sequence elements
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static <T> T[] concat(T /*@Nullable*/ [] a, /*@Nullable*/ List<T> b) {
    if (a == null) {
      if (b != null) {
        return toTArray(b);
      } else {
        @SuppressWarnings("unchecked")
        T[] result = (T[]) new Object[0];
        return result;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        @SuppressWarnings("unchecked")
        T[] result = (T[]) new /*@MonotonicNonNull*/ Object[a.length + b.size()];

        System.arraycopy(a, 0, result, 0, a.length);
        // System.arraycopy(b, 0, result, a.length, b.size());
        for (int i = 0; i < b.size(); i++) {
          result[i + a.length] = b.get(i);
        }
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param <T> the type of the sequence elements
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static <T> T[] concat(/*@Nullable*/ List<T> a, T /*@Nullable*/ [] b) {
    if (a == null) {
      if (b != null) {
        return b;
      } else {
        @SuppressWarnings("unchecked")
        T[] result = (T[]) new Object[0];
        return result;
      }
    } else {
      if (b == null) {
        return toTArray(a);
      } else {
        @SuppressWarnings("unchecked")
        T[] result = (T[]) new /*@MonotonicNonNull*/ Object[a.size() + b.length];

        // System.arraycopy(a, 0, result, 0, a.size());
        for (int i = 0; i < a.size(); i++) {
          result[i] = a.get(i);
        }
        System.arraycopy(b, 0, result, a.size(), b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param <T> the type of the sequence elements
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static <T> T[] concat(/*@Nullable*/ List<T> a, /*@Nullable*/ List<T> b) {
    if (a == null) {
      if (b != null) {
        return toTArray(b);
      } else {
        @SuppressWarnings("unchecked")
        T[] result = (T[]) new Object[0];
        return result;
      }
    } else {
      if (b == null) {
        return toTArray(a);
      } else {
        @SuppressWarnings("unchecked")
        T[] result = (T[]) new /*@MonotonicNonNull*/ Object[a.size() + b.size()];

        // System.arraycopy(a, 0, result, 0, a.length);
        for (int i = 0; i < a.size(); i++) {
          result[i] = a.get(i);
        }
        // System.arraycopy(b, 0, result, a.length, b.length);
        for (int i = 0; i < b.size(); i++) {
          result[i + a.size()] = b.get(i);
        }
        return result;
      }
    }
  }

  // Note: PolyAll is not quite right.  Need to review.
  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first array to concatenate
   * @param b the second array to concatenate
   * @return an array that concatenates the arguments
   */
  public static /*@PolyAll*/ String[] concat(
      /*@PolyAll*/ String /*@Nullable*/ [] a, /*@PolyAll*/ String /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new String[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        /*@PolyAll*/ String[] result = new String[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static byte[] concat(byte /*@Nullable*/ [] a, byte /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new byte[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        byte[] result = new byte[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static boolean[] concat(boolean /*@Nullable*/ [] a, boolean /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new boolean[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        boolean[] result = new boolean[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static char[] concat(char /*@Nullable*/ [] a, char /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new char[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        char[] result = new char[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static double[] concat(double /*@Nullable*/ [] a, double /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new double[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        double[] result = new double[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static float[] concat(float /*@Nullable*/ [] a, float /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new float[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        float[] result = new float[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static int[] concat(int /*@Nullable*/ [] a, int /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new int[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        int[] result = new int[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static long[] concat(long /*@Nullable*/ [] a, long /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new long[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        long[] result = new long[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  /**
   * Return an array that contains all the elements of both argument arrays, in order. Returns a new
   * array unless one argument is null, in which case it returns the other array.
   *
   * @param a the first sequence to concatenate
   * @param b the second sequence to concatenate
   * @return an array that concatenates the arguments
   */
  public static short[] concat(short /*@Nullable*/ [] a, short /*@Nullable*/ [] b) {
    if (a == null) {
      if (b == null) {
        return new short[0];
      } else {
        return b;
      }
    } else {
      if (b == null) {
        return a;
      } else {
        short[] result = new short[a.length + b.length];

        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Printing
  ///

  // This should be extended to all types, when I get around to it.  The
  // methods are patterned after that of java.util.Vector (and use its
  // output format).

  /**
   * Return a string representation of the array. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a an array
   * @return a string representation of the array
   * @see java.util.Vector#toString
   */
  /*@SideEffectFree*/
  public static String toString(/*@PolyAll*/ Object /*@Nullable*/ [] a) {
    return toString(a, false);
  }

  /**
   * Return a string representation of the array. The representation is patterned after that of
   * java.util.Vector. Furthermore, each element is quoted like a Java String.
   *
   * @param a an array
   * @return a string representation of the array, with the elements quoted
   * @see java.util.Vector#toString
   */
  /*@SideEffectFree*/
  public static String toStringQuoted(/*@PolyAll*/ Object /*@Nullable*/ [] a) {
    return toString(a, true);
  }

  /**
   * Return a string representation of the array. The representation is patterned after that of
   * java.util.Vector. Furthermore, if quoted is true, then each element is quoted like a Java
   * String.
   *
   * @param a an array
   * @param quoted whether to quote the array elements
   * @return a string representation of the array
   * @see java.util.Vector#toString
   */
  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public static String toString(/*@PolyAll*/ Object /*@Nullable*/ [] a, boolean quoted) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i = 1; i < a.length; i++) {
        sb.append(", ");
        if (quoted && a[i] instanceof String) {
          String elt = (String) a[i];
          sb.append('\"');
          sb.append(UtilMDE.escapeNonJava(elt));
          sb.append('\"');
        } else {
          sb.append(a[i]);
        }
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the collection. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a a collection
   * @return a string representation of the collection
   * @see java.util.Vector#toString
   */
  /*@SideEffectFree*/
  public static String toString(/*@Nullable*/ Collection<?> a) {
    return toString(a, false);
  }

  /**
   * Return a string representation of the collection. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a a collection
   * @return a string representation of the collection, with the elements quoted
   * @see java.util.Vector#toString
   */
  /*@SideEffectFree*/
  public static String toStringQuoted(/*@Nullable*/ Collection<?> a) {
    return toString(a, true);
  }

  /**
   * Return a string representation of the collection. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a a collection
   * @param quoted whether to quote the collection elements that are Java strings
   * @return a string representation of the list
   * @see java.util.Vector#toString
   */
  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public static String toString(/*@Nullable*/ Collection<?> a, boolean quoted) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.size() > 0) {
      Iterator<?> itor = a.iterator();
      sb.append(itor.next());
      while (itor.hasNext()) {
        sb.append(", ");
        Object elt = itor.next();
        if (quoted && elt instanceof String) {
          sb.append('\"');
          sb.append(UtilMDE.escapeNonJava((String) elt));
          sb.append('\"');
        } else {
          sb.append(elt);
        }
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the array. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a an array
   * @return a string representation of the array
   * @see java.util.Vector#toString
   */
  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public static String toString(int /*@Nullable*/ [] a) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i = 1; i < a.length; i++) {
        sb.append(", ");
        sb.append(a[i]);
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the array. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a an array
   * @return a string representation of the array
   * @see java.util.Vector#toString
   */
  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public static String toString(long /*@Nullable*/ [] a) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i = 1; i < a.length; i++) {
        sb.append(", ");
        sb.append(a[i]);
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the array. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a an array
   * @return a string representation of the array
   * @see java.util.Vector#toString
   */
  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public static String toString(double /*@Nullable*/ [] a) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i = 1; i < a.length; i++) {
        sb.append(", ");
        sb.append(a[i]);
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the array. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a an array
   * @return a string representation of the array
   * @see java.util.Vector#toString
   */
  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public static String toString(float /*@Nullable*/ [] a) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i = 1; i < a.length; i++) {
        sb.append(", ");
        sb.append(a[i]);
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the array. The representation is patterned after that of
   * java.util.Vector.
   *
   * @param a an array
   * @return a string representation of the array
   * @see java.util.Vector#toString
   */
  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public static String toString(boolean /*@Nullable*/ [] a) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i = 1; i < a.length; i++) {
        sb.append(", ");
        sb.append(a[i]);
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Casts obj down to the proper array type then calls the appropriate toString() method. Only call
   * this method if obj is a boolean, double, int, long, or Object array.
   *
   * @param obj an array
   * @return a string representation of the array
   * @throws IllegalArgumentException if obj is null or is not one of the types mentioned above
   */
  @SuppressWarnings("purity") // defensive coding: throw exception when argument is invalid
  /*@SideEffectFree*/
  public static String toString(Object obj) throws IllegalArgumentException {
    if (obj instanceof boolean[]) {
      return toString((boolean[]) obj);
    } else if (obj instanceof double[]) {
      return toString((double[]) obj);
    } else if (obj instanceof float[]) {
      return toString((float[]) obj);
    } else if (obj instanceof int[]) {
      return toString((int[]) obj);
    } else if (obj instanceof long[]) {
      return toString((long[]) obj);
    } else if (obj instanceof Object[]) {
      return toString((Object[]) obj);
    } else if (obj instanceof List<?>) {
      return toString((List<?>) obj);
    } else {
      throw new IllegalArgumentException(
          "Argument is " + ((obj == null) ? "null" : "of class " + obj.getClass().getName()));
    }
  }

  /**
   * Casts obj down to the proper array type then calls .length. Only call this method if obj is a
   * boolean, double, int, long, or Object array.
   *
   * @param obj a list
   * @return a string representation of the array
   * @throws IllegalArgumentException if obj is null or is not one of the types mentioned above
   */
  @SuppressWarnings("purity") // defensive coding: throw exception when argument is invalid
  /*@Pure*/
  public static int length(Object obj) throws IllegalArgumentException {
    if (obj instanceof boolean[]) {
      return ((boolean[]) obj).length;
    } else if (obj instanceof double[]) {
      return ((double[]) obj).length;
    } else if (obj instanceof int[]) {
      return ((int[]) obj).length;
    } else if (obj instanceof long[]) {
      return ((long[]) obj).length;
    } else if (obj instanceof Object[]) {
      return ((Object[]) obj).length;
    } else if (obj instanceof List<?>) {
      return ((List<?>) obj).size();
    } else {
      throw new IllegalArgumentException(
          "Argument is " + ((obj == null) ? "null" : "of class " + obj.getClass().getName()));
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Sortedness
  ///

  /**
   * Returns whether the array is sorted.
   *
   * @param a an array
   * @return true iff the array is sorted
   */
  /*@Pure*/
  public static boolean sorted(int[] a) {
    for (int i = 0; i < a.length - 1; i++) {
      if (a[i + 1] < a[i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Returns whether the array is sorted.
   *
   * @param a an array
   * @return true iff the array is sorted
   */
  /*@Pure*/
  public static boolean sorted(long[] a) {
    for (int i = 0; i < a.length - 1; i++) {
      if (a[i + 1] < a[i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Returns whether the array is sorted in desending order.
   *
   * @param a an array
   * @return true iff the array is sorted in desending order
   */
  /*@Pure*/
  public static boolean sorted_descending(int[] a) {
    for (int i = 0; i < a.length - 1; i++) {
      if (a[i + 1] > a[i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Returns whether the array is sorted in desending order.
   *
   * @param a an array
   * @return true iff the array is sorted in desending order
   */
  /*@Pure*/
  public static boolean sorted_descending(long[] a) {
    for (int i = 0; i < a.length - 1; i++) {
      if (a[i + 1] > a[i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(boolean[] a) {
    HashSet<Boolean> hs = new HashSet<Boolean>();
    for (int i = 0; i < a.length; i++) {
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      Boolean n = Boolean.valueOf(a[i]);
      if (hs.contains(n)) {
        return false;
      }
      hs.add(n);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(byte[] a) {
    HashSet<Byte> hs = new HashSet<Byte>();
    for (int i = 0; i < a.length; i++) {
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      Byte n = new Byte(a[i]);
      if (hs.contains(n)) {
        return false;
      }
      hs.add(n);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(char[] a) {
    HashSet<Character> hs = new HashSet<Character>();
    for (int i = 0; i < a.length; i++) {
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      Character n = new Character(a[i]);
      if (hs.contains(n)) {
        return false;
      }
      hs.add(n);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(float[] a) {
    HashSet<Float> hs = new HashSet<Float>();
    for (int i = 0; i < a.length; i++) {
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      Float n = new Float(a[i]);
      if (hs.contains(n)) {
        return false;
      }
      hs.add(n);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(short[] a) {
    HashSet<Short> hs = new HashSet<Short>();
    for (int i = 0; i < a.length; i++) {
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      Short n = new Short(a[i]);
      if (hs.contains(n)) {
        return false;
      }
      hs.add(n);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(int[] a) {
    HashSet<Integer> hs = new HashSet<Integer>();
    for (int i = 0; i < a.length; i++) {
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      Integer n = new Integer(a[i]);
      if (hs.contains(n)) {
        return false;
      }
      hs.add(n);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements. Equality checking uses {@link
   * Double#equals}.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(double[] a) {
    HashSet<Double> hs = new HashSet<Double>();
    for (int i = 0; i < a.length; i++) {
      // Could be optimized not to create the last element,
      // but that would make the code much less readable.
      Double n = new Double(a[i]);
      if (hs.contains(n)) {
        return false;
      }
      hs.add(n);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(long[] a) {
    HashSet<Long> hs = new HashSet<Long>();
    for (int i = 0; i < a.length; i++) {
      // Could be optimized not to create the last element,
      // but that would make the code much less readable.
      Long n = new Long(a[i]);
      if (hs.contains(n)) {
        return false;
      }
      hs.add(n);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(String[] a) {
    HashSet<String> hs = new HashSet<String>();
    for (int i = 0; i < a.length; i++) {
      if (hs.contains(a[i])) {
        return false;
      }
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      hs.add(a[i]);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param a an array
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean noDuplicates(Object[] a) {
    HashSet<Object> hs = new HashSet<Object>();
    for (int i = 0; i < a.length; i++) {
      if (hs.contains(a[i])) {
        return false;
      }
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      hs.add(a[i]);
    }
    return true;
  }

  /**
   * Return true iff a does not contain duplicate elements.
   *
   * <p>The implementation uses O(n) time and O(n) space.
   *
   * @param <T> the type of the elements
   * @param a a list
   * @return true iff a does not contain duplicate elements
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static <T> boolean noDuplicates(List<T> a) {
    HashSet<T> hs = new HashSet<T>();
    for (int i = 0; i < a.size(); i++) {
      if (hs.contains(a.get(i))) {
        return false;
      }
      // Could be optimized not to add the last element,
      // but that would make the code much less readable.
      hs.add(a.get(i));
    }
    return true;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Arrays as partial functions of int->int
  ///

  /**
   * Returns true if the array is a permutation of [0..a.length).
   *
   * @param a an array, representing a function
   * @return true iff all elements of a are in [0..a.length) and a contains no duplicates.
   */
  @SuppressWarnings("purity") // side effect to local state (array)
  /*@Pure*/
  public static boolean fn_is_permutation(int[] a) {
    // In the common case we expect to succeed, so use as few loops as possible
    boolean[] see = new boolean[a.length];
    for (int i = 0; i < a.length; i++) {
      int n = a[i];
      if (n < 0 || n >= a.length || see[n]) {
        return false;
      }
      see[n] = true;
    }
    return true;
  }

  /**
   * Return true iff the array does not contain -1.
   *
   * @param a an array, representing a function
   * @return true iff no element of a maps to -1
   */
  /*@Pure*/
  public static boolean fn_is_total(int[] a) {
    return indexOf(a, -1) == -1; // not found
  }

  /**
   * Return an array [0..lenth).
   *
   * @param length the length of the result
   * @return fresh array that is the identity function of the given length
   */
  public static int[] fn_identity(int length) {
    int[] result = new int[length];
    for (int i = 0; i < length; i++) {
      result[i] = i;
    }
    return result;
  }

  /**
   * Requires that fn_is_permutation(a) holds.
   *
   * @param a the input permutation
   * @return fresh array which is the inverse of the given permutation
   * @see #fn_is_permutation(int[])
   */
  public static int[] fn_inverse_permutation(int[] a) {
    return fn_inverse(a, a.length);
  }

  /**
   * Return the inverse of the given function, which is represented as an array.
   *
   * @param a an array representing a function from [0..a.length) to [0..arange); each element of a
   *     is between 0 and arange inclusive
   * @param arange length of the argument's range and the result's domain
   * @return function from [0..arange) to [0..a.length) that is the inverse of a
   * @throws IllegalArgumentException if a value of a is outside of arange
   * @exception UnsupportedOperationException when the function is not invertible
   */
  public static int[] fn_inverse(int[] a, int arange) {
    int[] result = new int[arange];
    Arrays.fill(result, -1);
    for (int i = 0; i < a.length; i++) {
      int ai = a[i];
      if (ai < -1 || ai >= arange) {
        throw new IllegalArgumentException(String.format("Bad range value: a[%d]=%d", i, ai));
      }
      // ai is either -1 or a valid index
      if (ai >= 0) {
        if (result[ai] != -1) {
          throw new UnsupportedOperationException(
              String.format("Not invertible; a[%d]=%d and a[%d]=%d", result[ai], ai, i, ai));
        }
        result[ai] = i;
      }
    }
    return result;
  }

  /**
   * Return the composition of the given two functions, all of which are represented as arrays.
   *
   * @param a function from [0..a.length) to [0..b.length)
   * @param b function from [0..b.length) to range R
   * @return function from [0..a.length) to range R that is the composition of a and b
   */
  public static int[] fn_compose(int[] a, int[] b) {
    int[] result = new int[a.length];
    for (int i = 0; i < a.length; i++) {
      int inner = a[i];
      if (inner == -1) {
        result[i] = -1;
      } else {
        result[i] = b[inner];
      }
    }
    return result;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Set operations, such as subset, unions, and intersections
  ///

  // This implementation is O(n^2) when the smaller really is a subset, but
  // might be quicker when it is not.  Sorting both sets has (minimum
  // and maximum) running time of Theta(n log n).
  /**
   * Return whether smaller is a subset of bigger.
   *
   * <p>The implementation is to use collections because we want to take advantage of HashSet's
   * constant time membership tests.
   *
   * @param smaller first set to test
   * @param bigger second set to test
   * @return true iff smaller is a subset of bigger
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean isSubset(long[] smaller, long[] bigger) {
    Set<Long> setBigger = new HashSet<Long>();

    for (int i = 0; i < bigger.length; i++) {
      setBigger.add(new Long(bigger[i]));
    }

    for (int i = 0; i < smaller.length; i++) {
      Long elt = new Long(smaller[i]);
      if (!setBigger.contains(elt)) {
        return false;
      }
    }

    return true;
  }

  // This implementation is O(n^2) when the smaller really is a subset, but
  // might be quicker when it is not.  Sorting both sets has (minimum
  // and maximum) running time of Theta(n log n).
  /**
   * Return whether smaller is a subset of bigger.
   *
   * <p>The implementation is to use collections because we want to take advantage of HashSet's
   * constant time membership tests.
   *
   * @param smaller first set to test
   * @param bigger second set to test
   * @return true iff smaller is a subset of bigger
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean isSubset(double[] smaller, double[] bigger) {
    Set<Double> setBigger = new HashSet<Double>();

    for (int i = 0; i < bigger.length; i++) {
      setBigger.add(new Double(bigger[i]));
    }

    for (int i = 0; i < smaller.length; i++) {
      Double elt = new Double(smaller[i]);
      if (!setBigger.contains(elt)) {
        return false;
      }
    }

    return true;
  }

  // This implementation is O(n^2) when the smaller really is a subset, but
  // might be quicker when it is not.  Sorting both sets has (minimum
  // and maximum) running time of Theta(n log n).
  /**
   * Return whether smaller is a subset of bigger.
   *
   * <p>The implementation is to use collections because we want to take advantage of HashSet's
   * constant time membership tests.
   *
   * @param smaller first set to test
   * @param bigger second set to test
   * @return true iff smaller is a subset of bigger
   */
  @SuppressWarnings("purity") // side effect to local state (HashSet)
  /*@Pure*/
  public static boolean isSubset(String[] smaller, String[] bigger) {
    Set<String> setBigger = new HashSet<String>();

    for (int i = 0; i < bigger.length; i++) {
      setBigger.add(bigger[i]);
    }

    for (int i = 0; i < smaller.length; i++) {
      if (!setBigger.contains(smaller[i])) {
        return false;
      }
    }

    return true;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Array comparators
  ///

  /**
   * Compare two arrays lexically (element-by-element). If all shared elements are the same, but the
   * lengths differ, then the shorter array is considered less.
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical numbers).
   */
  public static final class IntArrayComparatorLexical implements Comparator<int[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    public int compare(int[] a1, int[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int len = Math.min(a1.length, a2.length);
      for (int i = 0; i < len; i++) {
        if (a1[i] != a2[i]) {
          return ((a1[i] > a2[i]) ? 1 : -1);
        }
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Compare two arrays lexically (element-by-element). If all shared elements are the same, but the
   * lengths differ, then the shorter array is considered less.
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical numbers).
   */
  public static final class LongArrayComparatorLexical implements Comparator<long[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    public int compare(long[] a1, long[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int len = Math.min(a1.length, a2.length);
      for (int i = 0; i < len; i++) {
        if (a1[i] != a2[i]) {
          return ((a1[i] > a2[i]) ? 1 : -1);
        }
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Compare two arrays lexically (element-by-element). If all shared elements are the same, but the
   * lengths differ, then the shorter array is considered less.
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical numbers).
   */
  public static final class DoubleArrayComparatorLexical
      implements Comparator<double[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    public int compare(double[] a1, double[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int len = Math.min(a1.length, a2.length);
      for (int i = 0; i < len; i++) {
        int result = Double.compare(a1[i], a2[i]);
        if (result != 0) {
          return (result);
        }
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Compare two arrays lexically (element-by-element). If all shared elements are the same, but the
   * lengths differ, then the shorter array is considered less.
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical Strings).
   */
  public static final class StringArrayComparatorLexical
      implements Comparator<String[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    @SuppressWarnings(
        "override.param.invalid") // CF bug: does not permit expanding annotations on array elements with @Poly
    // The signature on this method is unnecessarily strict because it
    // requires that the component types be identical.  The signature should
    // be compare(@PolyAll(1) String[], @PolyAll(2) String[]), but the
    // @PolyAll qualifier does not yet take an argument.
    public int compare(/*@PolyAll*/ String[] a1, /*@PolyAll*/ String[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int len = Math.min(a1.length, a2.length);
      for (int i = 0; i < len; i++) {
        int tmp = 0;
        if ((a1[i] == null) && (a2[i] == null)) {
          tmp = 0;
        } else if (a1[i] == null) {
          tmp = -1;
        } else if (a2[i] == null) {
          tmp = 1;
        } else {
          tmp = a1[i].compareTo(a2[i]);
        }
        if (tmp != 0) {
          return (tmp);
        }
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Compare two arrays lexically (element-by-element). If all shared elements are the same, but the
   * lengths differ, then the shorter array is considered less.
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical objects).
   */
  public static final class ComparableArrayComparatorLexical<T extends Comparable<T>>
      implements Comparator<T[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    @SuppressWarnings(
        "override.param.invalid") // CF bug: does not permit expanding annotations on array elements with @Poly
    // The signature on this method is unnecessarily strict because it
    // requires that the component types be identical.  The signature should
    // be compare(@PolyAll(1) T[], @PolyAll(2) T[]), but the
    // @PolyAll qualifier does not yet take an argument.
    public int compare(/*@PolyAll*/ T[] a1, /*@PolyAll*/ T[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int len = Math.min(a1.length, a2.length);
      for (int i = 0; i < len; i++) {
        T elt1 = a1[i];
        T elt2 = a2[i];
        // Make null compare smaller than anything else
        if ((elt1 == null) && (elt2 == null)) {
          continue;
        }
        if (elt1 == null) {
          return -1;
        }
        if (elt2 == null) {
          return 1;
        }
        int tmp = elt1.compareTo(elt2);
        if (tmp != 0) {
          return tmp;
        }
        // Check the assumption that the two elements are equal.
        assert elt1.equals(elt2);
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Compare two arrays lexically (element-by-element). If all shared elements are the same, but the
   * lengths differ, then the shorter array is considered less.
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical objects).
   */
  public static final class ObjectArrayComparatorLexical
      implements Comparator<Object[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    @SuppressWarnings(
        "override.param.invalid") // CF bug: does not permit expanding annotations on array elements with @Poly
    // The signature on this method is unnecessarily strict because it
    // requires that the component types be identical.  The signature should
    // be compare(@PolyAll(1) Object[], @PolyAll(2) Object[]), but the
    // @PolyAll qualifier does not yet take an argument.
    public int compare(/*@PolyAll*/ Object[] a1, /*@PolyAll*/ Object[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int len = Math.min(a1.length, a2.length);
      for (int i = 0; i < len; i++) {
        Object elt1 = a1[i];
        Object elt2 = a2[i];
        // Make null compare smaller than anything else
        if ((elt1 == null) && (elt2 == null)) {
          continue;
        }
        if (elt1 == null) {
          return -1;
        }
        if (elt2 == null) {
          return 1;
        }
        int tmp = elt1.hashCode() - elt2.hashCode();
        if (tmp != 0) {
          return tmp;
        }
        // I'm counting on the fact that hashCode returns a different
        // number for each Object in the system.  This checks that assumption.
        assert elt1.equals(elt2);
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Compare two arrays first by length (a shorter array is considered less), and if of equal length
   * compare lexically (element-by-element).
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical numbers).
   */
  public static final class IntArrayComparatorLengthFirst
      implements Comparator<int[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays by length, then lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    public int compare(int[] a1, int[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int tmp;
      tmp = a1.length - a2.length;
      if (tmp != 0) {
        return tmp;
      }
      for (int i = 0; i < a1.length; i++) {
        if (a1[i] != a2[i]) {
          return ((a1[i] > a2[i]) ? 1 : -1);
        }
      }
      return 0;
    }
  }

  /**
   * Compare two arrays first by length (a shorter array is considered less), and if of equal length
   * compare lexically (element-by-element).
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical numbers).
   */
  public static final class LongArrayComparatorLengthFirst
      implements Comparator<long[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays by length, then lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    public int compare(long[] a1, long[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int lendiff = a1.length - a2.length;
      if (lendiff != 0) {
        return lendiff;
      }
      long tmp;
      for (int i = 0; i < a1.length; i++) {
        if (a1[i] != a2[i]) {
          return ((a1[i] > a2[i]) ? 1 : -1);
        }
      }
      return 0;
    }
  }

  /**
   * Compare two arrays first by length (a shorter array is considered less), and if of equal length
   * compare lexically (element-by-element).
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical objects).
   */
  public static final class ComparableArrayComparatorLengthFirst<T extends Comparable<T>>
      implements Comparator<T[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays by length, then lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    @SuppressWarnings(
        "override.param.invalid") // CF bug: does not permit expanding annotations on array elements with @Poly
    // The signature on this method is unnecessarily strict because it
    // requires that the component types be identical.  The signature should
    // be compare(@PolyAll(1) T[], @PolyAll(2) T[]), but the
    // @PolyAll qualifier does not yet take an argument.
    public int compare(/*@PolyAll*/ T[] a1, /*@PolyAll*/ T[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int tmp;
      tmp = a1.length - a2.length;
      if (tmp != 0) {
        return tmp;
      }
      for (int i = 0; i < a1.length; i++) {
        T elt1 = a1[i];
        T elt2 = a2[i];
        // Make null compare smaller than anything else
        if ((elt1 == null) && (elt2 == null)) {
          continue;
        }
        if (elt1 == null) {
          return -1;
        }
        if (elt2 == null) {
          return 1;
        }
        tmp = elt1.compareTo(elt2);
        if (tmp != 0) {
          return tmp;
        }
        // Check the assumption that the two elements are equal.
        assert elt1.equals(elt2);
      }
      return 0;
    }
  }

  /**
   * Compare two arrays first by length (a shorter array is considered less), and if of equal length
   * compare lexically (element-by-element).
   *
   * <p>Note: this comparator imposes orderings that are inconsistent with {@link Object#equals}.
   * That is, it may return 0 if the arrays are not equal according to {@code equals()} (but do
   * contain identical objects).
   */
  public static final class ObjectArrayComparatorLengthFirst
      implements Comparator<Object[]>, Serializable {
    static final long serialVersionUID = 20150812L;

    /**
     * Compare two arrays by length, then lexically (element-by-element).
     *
     * @param a1 first array to compare
     * @param a2 first array to compare
     * @return a negative integer, zero, or a positive integer, depending on whether the first
     *     argument is less than, equal to, or greater than the second argument
     */
    /*@Pure*/
    @SuppressWarnings(
        "override.param.invalid") // CF bug: does not permit expanding annotations on array elements with @Poly
    // The signature on this method is unnecessarily strict because it
    // requires that the component types be identical.  The signature should
    // be compare(@PolyAll(1) Object[], @PolyAll(2) Object[]), but the
    // @PolyAll qualifier does not yet take an argument.
    public int compare(/*@PolyAll*/ Object[] a1, /*@PolyAll*/ Object[] a2) {
      if (a1 == a2) {
        return 0;
      }
      int tmp;
      tmp = a1.length - a2.length;
      if (tmp != 0) {
        return tmp;
      }
      for (int i = 0; i < a1.length; i++) {
        Object elt1 = a1[i];
        Object elt2 = a2[i];
        // Make null compare smaller than anything else
        if ((elt1 == null) && (elt2 == null)) {
          continue;
        }
        if (elt1 == null) {
          return -1;
        }
        if (elt2 == null) {
          return 1;
        }
        tmp = elt1.hashCode() - elt2.hashCode();
        if (tmp != 0) {
          return tmp;
        }
        // I'm counting on the fact that hashCode returns a different
        // number for each Object in the system.  This checks that assumption.
        assert elt1.equals(elt2);
      }
      return 0;
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// nullness
  ///

  /**
   * Return true if a contains null.
   *
   * @param a an array
   * @return true iff some element of a is null (false if a is zero-sized)
   */
  /*@Pure*/
  public static boolean any_null(/*@PolyAll*/ Object[] a) {
    if (a.length == 0) {
      return false;
    }
    // The cast ensures that the right version of IndexOfEq gets called.
    return indexOfEq(a, (/*@Nullable*/ Object) null) >= 0;
  }

  /**
   * Return true if all elements of a are null.
   *
   * @param a an array
   * @return true iff all elements of a are null (unspecified result if a is zero-sized)
   */
  /*@Pure*/
  public static boolean all_null(/*@PolyAll*/ Object[] a) {
    for (int i = 0; i < a.length; i++) {
      if (!(a[i] == null)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Return true if a contains null.
   *
   * @param a an array
   * @return true iff some element of a is null (false if a is zero-sized)
   */
  /*@Pure*/
  public static boolean any_null(List<?> a) {
    if (a.size() == 0) {
      return false;
    }
    // The cast ensures that the right version of IndexOfEq gets called.
    return indexOfEq(a, (/*@Nullable*/ Object) null) >= 0;
  }

  /**
   * Return true if all elements of a are null.
   *
   * @param a an array
   * @return true iff all elements of a are null (unspecified result if a is zero-sized)
   */
  /*@Pure*/
  public static boolean all_null(List<?> a) {
    for (int i = 0; i < a.size(); i++) {
      if (!(a.get(i) == null)) {
        return false;
      }
    }
    return true;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// javadoc hacks
  ///

  // Without this, the Javadoc mentions of "java.util.Vector", are not links
  // in the generated HTML.
  // "private static Vector v;" doesn't work, nor does
  // "static { new java.util.Vector(); }", nor does "private Vector v".
  // Yuck!
  public Vector<?> javadocLossage;
}
