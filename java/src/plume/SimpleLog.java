package plume;

import java.io.PrintStream;
import java.util.ArrayDeque;

/*>>>
import org.checkerframework.checker.formatter.qual.*;
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * A logging class with the following features:
 *
 * <ul>
 *   <li>Can be enabled and disabled (when disabled, all operations are no-ops),
 *   <li>Write to a file or to standard output,
 *   <li>Can start and stop a timer, and nest timers,
 *   <li>Can enter and exit subtasks (their output is indented, and they are timed),
 *   <li>Can provide a backtrace (optionally provide a backtrace at every output), and
 *   <li>Can add newlines where appropriate, if variable line_oriented is set.
 * </ul>
 */
public final class SimpleLog {

  /** If false, do no output. */
  public boolean enabled;

  /** Always provide a backtrace (traceback) when calling {@code log()}. */
  public boolean always_traceback = false;

  /** Where to write logging output. Null if nothing has been output yet. */
  private /*@MonotonicNonNull*/ PrintStream logfile = null;

  /** The file for logging output. If null or "-", System.out is used. */
  private /*@Nullable*/ String filename = null;

  /** The current indentation string. */
  private String indent_str = "";
  /** Indentation string for one level of indentation. */
  public final String INDENT_STR_ONE_LEVEL = "  ";

  /**
   * True if every log call is made with a complete line of text. False if a log call may contain
   * multiple lines, or if multiple log calls may be made, each with parts of a line; in this case,
   * you must manage line delimiters yourself.
   */
  public boolean line_oriented = true;

  public ArrayDeque<Long> start_times = new ArrayDeque<Long>();

  /**
   * Create a new SimpleLog object.
   *
   * @param enabled whether the logger starts out enabled
   * @param always_traceback whether to always provide a backtrace (traceback) when calling {@code
   *     log()}
   */
  public SimpleLog(boolean enabled, boolean always_traceback) {
    this.enabled = enabled;
    this.always_traceback = always_traceback;
    push_start_time();
  }

  public SimpleLog(boolean enabled) {
    this(enabled, false);
  }

  public SimpleLog() {
    this(true);
  }

  /**
   * Create a new SimpleLog object.
   *
   * @param filename file name, or use "-" or null for standard output
   * @param enabled whether the logger starts out enabled
   */
  public SimpleLog(String filename, boolean enabled) {
    this(enabled);
    this.filename = filename;
  }

  /** @param filename file name, or use "-" or null for standard output */
  public SimpleLog(String filename) {
    this(filename, true);
  }

  public boolean enabled() {
    return enabled;
  }

  /**
   * Set the private field logfile, based on the private field filename.
   *
   * <p>This creates the file if it does not exist. Therefore, this should be called lazily, when
   * output is performed. Otherwise, it would be annoying to create a zero-size logfile if no output
   * is ever performed.
   */
  /*@EnsuresNonNull("logfile")*/
  private void setLogfile() {
    if (logfile != null) {
      return;
    }
    if (filename == null || filename.equals("-")) {
      logfile = System.out;
    } else {
      try {
        logfile = new PrintStream(filename);
      } catch (Exception e) {
        throw new RuntimeException("Can't open " + filename, e);
      }
    }
  }

  /**
   * Log a message. Provide a backtrace (traceback) if variable always_traceback is set.
   *
   * @param format format string for message
   * @param args values to be substituted into format
   */
  /*@FormatMethod*/
  public void log(String format, /*@Nullable*/ Object... args) {

    if (enabled) {
      setLogfile();
      format = add_newline(format);
      logfile.print(indent_str);
      logfile.printf(format, args);
      if (always_traceback) {
        tb();
      }
    }
  }

  /**
   * Log a message, and provide a backtrace (traceback, or tb).
   *
   * @param format format string for message
   * @param args values to be substituted into format
   */
  /*@FormatMethod*/
  public void log_tb(String format, /*@Nullable*/ Object... args) {
    if (enabled) {
      setLogfile();
      log(format, args);
      tb();
    }
  }

  /** Print a backtrace (traceback, or tb) to the log. */
  public void tb() {
    if (enabled) {
      setLogfile();
      Throwable t = new Throwable();
      t.fillInStackTrace();
      StackTraceElement[] ste_arr = t.getStackTrace();
      for (int ii = 2; ii < ste_arr.length; ii++) {
        StackTraceElement ste = ste_arr[ii];
        logfile.printf("%s  %s%n", indent_str, ste);
      }
    }
  }

  /**
   * Helper method: add a newline if one isn't already there, and if variable line_oriented is set.
   */
  private String add_newline(String format) {

    if (!line_oriented) {
      return format;
    }

    if (format.endsWith("%n")) {
      return format;
    }

    return format + "%n";
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Indentation
  ///

  /** Indents by one level and pushes a corresponding start time. */
  public void indent() {
    if (enabled) {
      indent_str += INDENT_STR_ONE_LEVEL;
      push_start_time();
    }
  }

  /**
   * Prints to the log, then indents.
   *
   * @param format format string for message
   * @param args values to be substituted into format
   * @see #indent()
   */
  /*@FormatMethod*/
  public void indent(String format, /*@Nullable*/ Object... args) {
    if (enabled) {
      log(format, args);
      indent();
    }
  }

  /** Clears indent and start times and then pushes one start time. */
  public void clear() {
    if (enabled) {
      indent_str = "";
      start_times.clear();
      push_start_time();
    }
  }

  /**
   * Calls {@link #clear()} and then logs the specified message
   *
   * @param format format string for message
   * @param args values to be substituted into format
   */
  /*@FormatMethod*/
  public void clear(String format, /*@Nullable*/ Object... args) {
    if (enabled) {
      setLogfile();
      clear();
      log(format, args);
    }
  }

  /** Exdents: reduces indentation and pops a start time. */
  @SuppressWarnings("index") // correlated fields:  indent_str is non-empty if start_times is
  public void exdent() {
    if (enabled) {
      if (start_times.isEmpty()) {
        boolean old_always_traceback = always_traceback;
        always_traceback = true;
        setLogfile();
        log("Called exdent when indentation was 0.");
        always_traceback = old_always_traceback;
      } else {
        indent_str = indent_str.substring(0, indent_str.length() - INDENT_STR_ONE_LEVEL.length());
        pop_start_time();
      }
    }
  }

  /**
   * Prints the time (showing the time for the nested timer) and then calls {@link #exdent()}.
   *
   * @param format format string for message
   * @param args values to be substituted into format
   */
  /*@FormatMethod*/
  public void exdent_time(String format, /*@Nullable*/ Object... args) {
    if (enabled) {
      setLogfile();
      // This puts the time inside, not outside, the indentation.
      log_time(format, args);
      exdent();
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Timing
  ///

  /** This overwrites the current start time; it does not push a new one!! */
  public void reset_start_time() {
    if (enabled) {
      pop_start_time();
      push_start_time();
    }
  }

  /** Push a new start time onto the stack. */
  /*@RequiresNonNull("start_times")*/
  public void push_start_time(
      /*>>> @UnknownInitialization(SimpleLog.class) @Raw(SimpleLog.class) SimpleLog this*/) {
    if (enabled) {
      start_times.push(System.currentTimeMillis());
    }
  }

  /** Pops a start time from the stack. */
  public void pop_start_time() {
    start_times.pop();
  }

  /**
   * Writes the specified message and the elapsed time since the last call to start_time(). Does not
   * pop nor reset the current start time.
   *
   * @param format format string for message
   * @param args values to be substituted into format
   */
  /*@FormatMethod*/
  public void log_time(String format, /*@Nullable*/ Object... args) {

    if (enabled) {
      setLogfile();
      Long start_time = start_times.peek();
      if (start_time == null) {
        throw new Error("Too many pops before calling log_time");
      }
      long elapsed = System.currentTimeMillis() - start_time.longValue();
      logfile.print(indent_str);
      if (elapsed > 1000) {
        logfile.printf("[%,f secs] ", elapsed / 1000.0);
      } else {
        logfile.print("[" + elapsed + " ms] ");
      }
      format = add_newline(format);
      logfile.printf(format, args);
    }
  }
}
