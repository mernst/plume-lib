package plume;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/*>>>
import checkers.nullness.quals.*;
import checkers.regex.quals.*;
import checkers.quals.EnsuresQualifierIf;
import dataflow.quals.Pure;
*/

// This class should be kept in sync with checkers.regex.RegexUtil .

/**
 * Utility methods for regular expressions, most notably for testing whether
 * a string is a regular expression.
 * <p>
 *
 * For an example of intended use, see section <a
 * href="http://types.cs.washington.edu/checker-framework/current/checkers-manual.html#regexutil-methods">Testing
 * whether a string is a regular expression</a> in the Checker Framework
 * manual.
 */
public class RegexUtil {

  /**
   * A checked version of {@link PatternSyntaxException}.
   * <p>
   * This exception is useful when an illegal regex is detected but the
   * contextual information to report a helpful error message is not available
   * at the current depth in the call stack. By using a checked
   * PatternSyntaxException the error must be handled up the call stack where
   * a better error message can be reported.
   * <p>
   *
   * Typical usage is:
   * <pre>
   * void myMethod(...) throws CheckedPatternSyntaxException {
   *   ...
   *   if (! isRegex(myString)) {
   *     throw new CheckedPatternSyntaxException(...);
   *   }
   *   ... Pattern.compile(myString) ...
   * </pre>
   *
   * Simply calling <tt>Pattern.compile</tt> would have a similar effect,
   * in that <tt>PatternSyntaxException</tt> would be thrown at run time if
   * <tt>myString</tt> is not a regular expression.  There are two problems
   * with such an approach.  First, a client of <tt>myMethod</tt> might
   * forget to handle the exception, since <tt>PatternSyntaxException</tt>
   * is not checked.  Also, the Regex Checker would issue a warning about
   * the call to <tt>Pattern.compile</tt> that might throw an exception.
   * The above usage pattern avoids both problems.
   *
   * @see PatternSyntaxException
   */
  public static class CheckedPatternSyntaxException extends Exception {

    private static final long serialVersionUID = 6266881831979001480L;

    private final PatternSyntaxException pse;

    /**
     * Constructs a new CheckedPatternSyntaxException equivalent to the
     * given {@link PatternSyntaxException}.
     * <p>
     * Consider calling this constructor with the result of
     * {@link RegexUtil#regexError}.
     * @param pse the PatternSyntaxException to be wrapped
     */
    public CheckedPatternSyntaxException(PatternSyntaxException pse) {
      this.pse = pse;
    }

    /**
     * Constructs a new CheckedPatternSyntaxException.
     *
     * @param desc A description of the error
     * @param regex The erroneous pattern
     * @param index The approximate index in the pattern of the error,
     *              or {@code -1} if the index is not known
     */
    public CheckedPatternSyntaxException(String desc, String regex, int index) {
      this(new PatternSyntaxException(desc, regex, index));
    }

    /**
     * Retrieves the description of the error.
     *
     * @return The description of the error
     */
    public String getDescription() {
      return pse.getDescription();
    }

    /**
     * Retrieves the error index.
     *
     * @return The approximate index in the pattern of the error, or {@code -1}
     *         if the index is not known
     */
    public int getIndex() {
      return pse.getIndex();
    }

    /**
     * Returns a multi-line string containing the description of the syntax
     * error and its index, the erroneous regular-expression pattern, and a
     * visual indication of the error index within the pattern.
     *
     * @return The full detail message
     */
    public String getMessage() {
      return pse.getMessage();
    }

    /**
     * Retrieves the erroneous regular-expression pattern.
     *
     * @return The erroneous pattern
     */
    public String getPattern() {
      return pse.getPattern();
    }
  }

  private RegexUtil() {
    throw new AssertionError("Class RegexUtil shouldn't be instantiated");
  }

  /**
   * Returns true if the argument is a syntactically valid regular
   * expression.
   * @param s string to check for being a regular expression
   * @return true iff s is a regular expression
   */
  /*@EnsuresQualifierIf(result=true, expression="#1", qualifier=Regex.class)*/
  /*@Pure*/ public static boolean isRegex(final String s) {
    return isRegex(s, 0);
  }

  /**
   * Returns true if the argument is a syntactically valid regular
   * expression with at least the given number of groups.
   * @param s string to check for being a regular expression
   * @param groups number of groups expected
   * @return true iff s is a regular expression with groups groups
   */
  /*>>>
  @SuppressWarnings({"regex","deterministic"})    // RegexUtil; for purity, catches an exception
  @Pure
  @EnsuresQualifierIf(result=true, expression="#1", qualifier=Regex.class)
  */
  public static boolean isRegex(String s, int groups) {
    Pattern p;
    try {
      p = Pattern.compile(s);
    } catch (PatternSyntaxException e) {
      return false;
    }
    return getGroupCount(p) >= groups;
  }

  /**
   * Returns true if the argument is a syntactically valid regular
   * expression.
   * @param c char to check for being a regular expression
   * @return true iff c is a regular expression
   */
  /*>>>
  @SuppressWarnings("regex")    // RegexUtil
  @Pure
  @EnsuresQualifierIf(result=true, expression="#1", qualifier=Regex.class)
  */
  public static boolean isRegex(char c) {
    return isRegex(Character.toString(c));
  }

  /**
   * Returns null if the argument is a syntactically valid regular
   * expression. Otherwise returns a string describing why the argument is
   * not a regex.
   * @param s string to check for being a regular expression
   * @return null, or a string describing why the argument is not a regex.
   */
  /*>>>
  @SuppressWarnings("regex")    // RegexUtil
  @SideEffectFree
  */
  public static /*@Nullable*/ String regexError(String s) {
    return regexError(s, 0);
  }

  /**
   * Returns null if the argument is a syntactically valid regular
   * expression with at least the given number of groups. Otherwise returns
   * a string describing why the argument is not a regex.
   * @param s string to check for being a regular expression
   * @param groups number of groups expected
   * @return null, or a string describing why the argument is not a regex.
   */
  /*>>>
  @SuppressWarnings({"regex","not.sef"})    // RegexUtil; 
  */
  /*@SideEffectFree*/
  public static /*@Nullable*/ String regexError(String s, int groups) {
    try {
      Pattern p = Pattern.compile(s);
      int actualGroups = getGroupCount(p);
      if (actualGroups < groups) {
        return regexErrorMessage(s, groups, actualGroups);
      }
    } catch (PatternSyntaxException e) {
      return e.getMessage();
    }
    return null;
  }

  /**
   * Returns null if the argument is a syntactically valid regular
   * expression. Otherwise returns a PatternSyntaxException describing
   * why the argument is not a regex.
   * @param s string to check for being a regular expression
   * @return null, or a PatternSyntaxException describing why the argument is not a regex.
   */
  /*>>>
  @SuppressWarnings("regex")    // RegexUtil
  @SideEffectFree
  */
  public static /*@Nullable*/ PatternSyntaxException regexException(String s) {
    return regexException(s, 0);
  }

  /**
   * Returns null if the argument is a syntactically valid regular
   * expression with at least the given number of groups. Otherwise returns a
   * PatternSyntaxException describing why the argument is not a regex.
   * @param s string to check for being a regular expression
   * @param groups number of groups expected
   * @return null, or a PatternSyntaxException describing why the argument is not a regex.
   */
  /*>>>
  @SuppressWarnings("regex")    // RegexUtil
  @SideEffectFree
  */
  public static /*@Nullable*/ PatternSyntaxException regexException(String s, int groups) {
    try {
      Pattern p = Pattern.compile(s);
      int actualGroups = getGroupCount(p);
      if (actualGroups < groups) {
        return new PatternSyntaxException(regexErrorMessage(s, groups, actualGroups), s, -1);
      }
    } catch (PatternSyntaxException pse) {
      return pse;
    }
    return null;
  }

  /**
   * Returns the argument as a {@code @Regex String} if it is a regex,
   * otherwise throws an error. The purpose of this method is to suppress Regex
   * Checker warnings. It should be very rarely needed.
   * @param s string to check for being a regular expression
   * @return its argument
   * @throws Error if argument is not a regex
   */
  /*@SideEffectFree*/
  public static /*@Regex*/ String asRegex(String s) {
    return asRegex(s, 0);
  }

  /**
   * Returns the argument as a {@code @Regex(groups) String} if it is a regex
   * with at least the given number of groups, otherwise throws an error. The
   * purpose of this method is to suppress Regex Checker warnings. It should
   * be very rarely needed.
   * @param s string to check for being a regular expression
   * @param groups number of groups expected
   * @return its argument
   * @throws Error if argument is not a regex
   */
  /*>>>
  @SuppressWarnings("regex")    // RegexUtil
  @SideEffectFree
  */
  public static /*@Regex*/ String asRegex(String s, int groups) {
    try {
      Pattern p = Pattern.compile(s);
      int actualGroups = getGroupCount(p);
      if (actualGroups < groups) {
        throw new Error(regexErrorMessage(s, groups, actualGroups));
      }
      return s;
    } catch (PatternSyntaxException e) {
      throw new Error(e);
    }
  }

  /**
   * Generates an error message for s when expectedGroups are needed, but s
   * only has actualGroups.
   * @param s string to check for being a regular expression
   * @return an error message for s when expectedGroups groups are needed, but s
   * only has actualGroups groups
   */
  /*@SideEffectFree*/
  private static String regexErrorMessage(String s, int expectedGroups, int actualGroups) {
    return "regex \"" + s + "\" has " + actualGroups + " groups, but " +
        expectedGroups + " groups are needed.";
  }

  /**
   * Return the count of groups in the argument.
   * @param p pattern whose groups to count
   * @return the count of groups in the argument
   */
  /*@SuppressWarnings("purity")*/     // does not depend on object identity
  /*@Pure*/
  private static int getGroupCount(Pattern p) {
    return p.matcher("").groupCount();
  }
}
