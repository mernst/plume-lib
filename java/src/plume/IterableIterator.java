// See http://www.rittau.org/blog/20061122-00

package plume;

import java.util.Iterator;

/**
 * In Java, Iterators are not Iterable, so they cannot be used in new-style for loops. This wrapper
 * works around that by making an Iterator that is also Iterable &mdash; that is, it implements the
 * iterator() method.
 *
 * <p>As an example, suppose that you have a class with a method:
 *
 * <pre>  public Iterator&lt;String&gt; backwardIterator() { ... }</pre>
 *
 * Because this method is not named {@code iterator()}, which probably has a different meaning than
 * {@code backwardIterator()}, it is not possible to use Java's new-style for loop (also known as
 * the foreach loop). Instead, a client must do:
 *
 * <pre>
 *  for (Iterator&lt;String&gt; itor = myObject.backwardIterator(); itor.hasNext(); ) {
 *    String element = itor.next();
 *    ...
 *  }
 * </pre>
 *
 * The {@code IterableIterator} class lets you write this more compactly:
 *
 * <pre>
 *  for (String element : new IterableIterator(myObject.backwardIterator())) {
 *     ...
 *  }
 * </pre>
 *
 * Another advantage of this syntax is that it explicitly indicates that the Iterator is not being
 * modified in the loop body, which is a common reason for not using the new-style for loop.
 *
 * <p>It's often better to use a real Iterable (e.g., a collections class) rather than an Iterator.
 * But in some cases the overhead is undesirable, or there are multiple ways to iterate so it
 * doesn't make sense to reserve the iterator() method for just one of them, or it is desirable to
 * use an Iterator because it throws a ConcurrentModificationException in case of errors, or for
 * other reasons. This class can be appropriate in such circumstances.
 *
 * <p><b>Warning:</b> Some clients might expect that calling Iterable.iterator() twice on a given
 * Iterable results in two objects that can both iterate over the whole sequence, and that won't
 * interfere with one another. That is not the case for this Iterable.
 *
 * @param <T> the element type of the IterableIterator
 */
public class IterableIterator<T> implements Iterable<T> {
  /** The Iterator that this is a wrapper around. */
  private Iterator<T> iter;

  /**
   * Create an IterableIterator.
   *
   * @param iter the iterator to wrap
   */
  public IterableIterator(Iterator<T> iter) {
    this.iter = iter;
  }

  /**
   * Return the iterator.
   *
   * @return the iterator
   */
  public Iterator<T> iterator() {
    return iter;
  }
}
