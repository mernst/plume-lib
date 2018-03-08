package plume;

import java.text.DecimalFormat;

/*>>>
import org.checkerframework.checker.index.qual.*;
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.nullness.qual.*;
*/

/** A simple class for recording elapsed time. */
public final class Stopwatch {

  long elapsedMillis = 0;
  long startTime = 0;

  /** When created, the stopwatch is running by default. */
  public Stopwatch() {
    this(true);
  }

  public Stopwatch(boolean start) {
    if (start) {
      start();
    }
  }

  /** Clears and restarts the stopwatch. */
  public void reset() {
    startTime = 0;
    elapsedMillis = 0;
    start();
  }

  /** Clears the stopwatch without restarting it. */
  public void clear() {
    startTime = 0;
    elapsedMillis = 0;
  }

  public void start(/*>>> @UnknownInitialization @Raw Stopwatch this*/) {
    if (startTime != 0) {
      throw new Error("Stopwatch is not stopped");
    }
    startTime = System.currentTimeMillis();
  }

  public void stop() {
    if (startTime == 0) {
      throw new Error("Stopwatch is not started");
    }
    elapsedMillis += (System.currentTimeMillis() - startTime);
    startTime = 0;
  }

  public long snapshot() {
    return (System.currentTimeMillis() - startTime);
  }

  public long elapsedMillis() {
    return elapsedMillis;
  }

  public double elapsedSeconds() {
    return elapsedMillis / 1000.0;
  }

  private static final DecimalFormat[] timeFormat = {
    new DecimalFormat("#.#"),
    new DecimalFormat("#.#"),
    new DecimalFormat("#.#"),
    new DecimalFormat("#.#"),
    new DecimalFormat("#.#"),
  };

  /**
   * Stops the watch, calculates the elapsed time, restarts the watch, and returns the elapsed time.
   *
   * @return a String representing the elapsed time
   */
  public String stop_start() {
    stop();
    String result = format();
    start();
    return (result);
  }

  public String format() {
    return format(1);
  }

  public String format(/*@IndexFor("Stopwatch.timeFormat")*/ int digits) {
    long runningMillis = elapsedMillis;
    if (startTime != 0) {
      runningMillis += (System.currentTimeMillis() - startTime);
    }
    return Stopwatch.timeFormat[digits].format(runningMillis / 1000.0) + "s";
  }
}
