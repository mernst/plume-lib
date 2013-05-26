package plume;


/** A Partitioner accepts Objects and assigns them to an equivalence
 * class.
 * @see MultiRandSelector
 */

public interface Partitioner<ELEMENT extends /*@Nullable*/ Object,CLASS extends /*@Nullable*/ Object> {

  /** @param obj the Object to be assigned to a bucket
   *  @return A key representing the bucket containing obj
   */
  public CLASS assignToBucket (ELEMENT obj);

}
