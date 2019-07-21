package plume;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * A Partitioner accepts Objects and assigns them to an equivalence class.
 *
 * @param <ELEMENT> the type of elements to be classified
 * @param <CLASS> the type of equivalence classes (classification buckets)
 * @see MultiRandSelector
 * @deprecated use <a
 *     href="http://plumelib.org/plume-util/api/org/plumelib/util/Partitioner.html">org.plumelib.util.Partitioner</a>
 */
@Deprecated // use org.plumelib.util.Partitioner
public interface Partitioner<
    ELEMENT extends /*@Nullable*/ Object, CLASS extends /*@Nullable*/ Object> {

  /**
   * @param obj the Object to be assigned to a bucket
   * @return a key representing the bucket containing obj
   */
  CLASS assignToBucket(ELEMENT obj);
}
