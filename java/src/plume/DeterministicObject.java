package plume;

// Even with these, Javadoc doesn't make links to HashMap or HashSet because
// the class's public interface does not use these classes.
// import java.util.HashMap;
// import java.util.HashSet;

/**
 * This is a deterministic version of the {@link Object} class.  To remove
 * one source of nondeterminism from your program, do not instantiate the
 * Object class, as in {@code new Object()}; instead do {@code new
 * DeterministicObject()}.
 * <p>
 *
 * This class differs from Object in that it overrides {@link #hashCode()}.
 * Any use of {@code Object.hashCode()} is nondeterministic because the
 * return value of {@code Object.hashCode()} depends on when the garbage
 * collector runs.  That nondeterminism can affect the iteration order of
 * {@link HashMap}s and {@link HashSet}s.
 */
public class DeterministicObject {

  /** The number of objects created so far. */
  static int counter = 0;

  /** The unique ID for this object. */
  final int id = counter++;

  /** Create a DeterministicObject. */
  public DeterministicObject() {
  }

  @Override
  public int hashCode() {
    return id;
  }

}
