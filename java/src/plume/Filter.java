package plume;

/**
 * Interface for things that make boolean decisions.
 * This is inspired by java.io.FilenameFilter.
 **/
public interface Filter<T extends @Nullable Object> {
  /**
   * Tests whether a specified Object satisfies the filter.
   * @param o the object to test
   * @return whether the object satisfies the filter
   */
  boolean accept(T o);
}
