package plume;

/*>>>
import org.checkerframework.checker.index.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

// NEEDS DOCUMENTATION!
// (Probably mostly Javadoc "see" directives, possibly with first line of relevant method doc.)

/**
 * Like StringBuilder, but adds a delimiter between each pair of strings that are inserted into the
 * Stringbuilder. This can simplify the logic of programs and also avoid errors.
 *
 * <p>Does not extend StringBuilder because that would probably break, due to the possibility of
 * calling the wrong version of append. Also, I don't (yet) want to override all the methods; this
 * simpler version seems sufficient for the time being.
 *
 * <p>Obsoleted by the Java 8 StringJoiner class.
 */
public class StringBuilderDelimited implements Appendable, CharSequence {

  /** The StringBuilder to which this delegates work. */
  private StringBuilder delegate = new StringBuilder();
  /** False iff some text has already been appended to this. */
  private boolean empty = true;
  /** The delimiter put between strings appended to this. */
  private final String delimiter;

  /**
   * Create a new StringBuilderDelimited.
   *
   * @param delimiter the delimiter to be put between strings that are appended to this
   */
  public StringBuilderDelimited(String delimiter) {
    this.delimiter = delimiter;
  }

  private void appendDelimiter(/*>>>@GuardSatisfied StringBuilderDelimited this*/) {
    if (empty) {
      empty = false;
    } else {
      delegate.append(delimiter);
    }
  }

  public StringBuilderDelimited append(/*@Nullable*/ String str) {
    appendDelimiter();
    delegate.append(str);
    return this;
  }

  public StringBuilderDelimited append(/*@Nullable*/ Object o) {
    appendDelimiter();
    delegate.append(o);
    return this;
  }

  @Override
  public StringBuilderDelimited append(char c) {
    appendDelimiter();
    delegate.append(c);
    return this;
  }

  @Override
  public StringBuilderDelimited append(/*@Nullable*/ CharSequence csq) {
    appendDelimiter();
    delegate.append(csq);
    return this;
  }

  @Override
  public StringBuilderDelimited append(
      /*@Nullable*/ CharSequence csq,
      /*@IndexOrHigh("#1")*/ int start,
      /*@IndexOrHigh("#1")*/ int end) {
    appendDelimiter();
    delegate.append(csq, start, end);
    return this;
  }

  @Override
  public char charAt(/*@IndexFor("this")*/ int index) {
    return delegate.charAt(index);
  }

  /*@Pure*/
  @Override
  @SuppressWarnings("index") // https://github.com/kelloggm/checker-framework/issues/175
  public /*@NonNegative*/ int length(/*>>>@GuardSatisfied StringBuilderDelimited this*/) {
    return delegate.length();
  }

  @Override
  public CharSequence subSequence(
      /*@IndexOrHigh("this")*/ int start, /*@IndexOrHigh("this")*/ int end) {
    return delegate.subSequence(start, end);
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied StringBuilderDelimited this*/) {
    return delegate.toString();
  }
}
