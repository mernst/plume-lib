package plume;

/*>>>
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringReader;

/**
 * Provides a somewhat simpler interface for tokenizing strings than does StreamTokenizer. All
 * tokenizing is done by StreamTokenizer.
 *
 * <p>The major difference from StreamTokenizer is that all tokens are returned as strings. EOF
 * returns a null, EOL returns an empty string. A delimiter is returned as a one-character string.
 * Words and numbers are returned as strings. Quoted strings are also returned as strings including
 * their quote characters (so they can easily be differentiated from words and numbers).
 *
 * <p>Other differences are:
 *
 * <ul>
 *   <li> Automatic setup for tokenizing strings.
 *   <li> Underscores are included in identifiers (words).
 *   <li> I/O errors (which should be impossible when tokenizing strings) are converted to
 *       RuntimeExceptions so that every call doesn't have to be included in a try block.
 *   <li> Convenience functions isWord(), isQString(), and need().
 *   <li> Returned string tokens are interned for easier comparisons.
 * </ul>
 */
public class StrTok {

  /** Where to read tokens from. */
  Reader reader;
  /** The StreamTokenizer to which all work is delegated. */
  public StreamTokenizer stok;
  /** How to handle errors. */
  ErrorHandler errorHandler;

  /**
   * Creates a tokenizer for the specified string.
   *
   * @param s string to tokenize
   */
  public StrTok(String s) {
    this(s, new ErrorHandler());
  }

  /**
   * Creates a tokenizer for the specified string with the specified error handler.
   *
   * @param s string to tokenize
   * @param e error handler
   */
  public StrTok(String s, ErrorHandler e) {
    reader = new StringReader(s);
    stok = new StreamTokenizer(reader);
    stok.wordChars('_', '_');
    set_error_handler(e);
  }

  /**
   * Default class for error handling. Throws a RuntimeException when an error occurs.
   *
   * @see #set_error_handler(ErrorHandler)
   */
  public static class ErrorHandler {

    /**
     * Called when an unexpected token is found (see {@link #need(String)}).
     *
     * @param s unexpected token that has been found
     */
    public void tok_error(String s) {
      throw new RuntimeException("StrTok error: " + s);
    }
  }

  /**
   * Returns the next token as a string. EOF returns a null, EOL returns an empty string. Delimiters
   * are returned as one character strings. Quoted strings and words are returned as strings.
   *
   * @return the next token
   */
  public /*@Nullable*/ /*@Interned*/ String nextToken() {

    // Get the next token.  Turn IO exceptions into runtime exceptions
    // so that callers don't have to catch them.
    try {
      stok.nextToken();
    } catch (Exception e) {
      throw new RuntimeException("StreamTokenizer exception: ", e);
    }

    return (token());
  }

  /** Causes the next call to nextToken() to return the current token. */
  public void pushBack() {
    stok.pushBack();
  }

  /**
   * Return the current token.
   *
   * @return the current token
   * @see #nextToken()
   */
  public /*@Nullable*/ /*@Interned*/ String token() {

    int ttype = stok.ttype;

    // Null indicates eof
    if (ttype == StreamTokenizer.TT_EOF) {
      return (null);
    }

    // Return end of line as an empty string
    if (ttype == StreamTokenizer.TT_EOL) {
      return ("");
    }

    // Return identifiers (words) and quoted strings.  Quoted strings
    // include their quote characters (for recognition)
    if (stok.sval != null) {
      if (ttype > 0) {
        String s = ((char) ttype) + stok.sval + ((char) ttype);
        return (s.intern());
      }
      return (stok.sval.intern());
    }

    // Other tokens are delimiters
    if (ttype > 0) {
      String s = "" + (char) ttype;
      return (s.intern());
    }

    throw new RuntimeException("Unexpected return " + ttype + " from StreamTokenizer");
  }

  /**
   * Specifies the single line comment character.
   *
   * @param ch the comment character
   * @see StreamTokenizer#commentChar(int)
   */
  public void commentChar(int ch) {
    stok.commentChar(ch);
  }

  /**
   * Specifies that matching pairs of this character delimit string constants.
   *
   * @param ch the quoting character
   * @see StreamTokenizer#quoteChar(int)
   */
  public void quoteChar(int ch) {
    stok.quoteChar(ch);
  }

  /**
   * Return the type of the current token.
   *
   * @return the type of the current token
   * @see StreamTokenizer#ttype
   */
  public int ttype() {
    return stok.ttype;
  }

  /**
   * Returns true if the current token is a word (identifier).
   *
   * @return true iff the current token is a word (identifier)
   */
  /*@Pure*/
  public boolean isWord() {
    return (stok.ttype == StreamTokenizer.TT_WORD);
  }

  /**
   * Returns true if the current token is a quoted string.
   *
   * @return true iff the current token is a quoted string
   */
  /*@Pure*/
  public boolean isQString() {
    return ((stok.sval != null) && (stok.ttype > 0));
  }

  /**
   * Sets the error handler. The default error handler will throw a runtime exception on errors.
   *
   * @param errorHandler the new error handler
   * @see ErrorHandler
   */
  /*@EnsuresNonNull("this.errorHandler")*/
  public void set_error_handler(
      /*>>>@UnknownInitialization @Raw StrTok this,*/ ErrorHandler errorHandler) {
    this.errorHandler = errorHandler;
  }

  /**
   * Reads the next token and checks that it matches tok. If it does not match, calls the current
   * error handling routine (see {@link #set_error_handler(StrTok.ErrorHandler)
   * set_error_handler()}). If it does match, just returns.
   *
   * @param tok string to check next token against
   */
  public void need(String tok) {

    String t = nextToken();
    if (tok.equals(t)) {
      return;
    }

    errorHandler.tok_error(String.format("Token %s found where %s expected", t, tok));
  }

  /**
   * Reads the next token and checks to make sure that it is a word (id). If it is not a word, calls
   * the error handling routine. If it is, returns the string of the word.
   *
   * @return next token, if if it a word; otherwise calls the error handling routine
   */
  public String need_word() {
    String t = nextToken();
    if (!isWord()) {
      errorHandler.tok_error(String.format("'%s' found where identifier expected", t));
    }
    assert t != null : "@AssumeAssertion(nullness): dependent: because of isWord check";
    return t;
  }
}
