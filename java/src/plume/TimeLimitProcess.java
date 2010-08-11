package plume;

import java.io.*;
import java.util.*;
import org.apache.commons.io.IOUtils;

/**
 * TimeLimitProcess is a subclass of Process such that the process is
 * killed if it runs for more than the specified number of milliseconds.
 * Wall clock seconds, not CPU seconds, are measured.
 * The process should already be started when TimeLimitProcess is invoked.
 * Typical use:
 * <pre>
 *   ProcessBuilder pb = ...;
 *   TimeLimitProcess p = new TimeLimitProcess(pb.start(), TIMEOUT_SEC * 1000);</pre>
 *
 * <b>Note</b>: If a Java process is destroyed (e.g., because it times
 * out), then its output is unreadable:  Java code trying to read its
 * output stream fails.  Here are two ways to get around this problem:
 *
 * <ul>
 * <li>
 * The client of TimeLimitProcess can send the process output to a file (or
 * ByteArrayOutputStream, etc.), which can be read after the process
 * terminates.  This is easy to do in Java 7, for example via
 * ProcessBuilder.redirectOutput(tempFile).  There does not appear to be an
 * easy way to do it in Java 6.
 * <li>
 * This class provides a workaround, in which it busy-waits reading the
 * standard and error outputs and stores them away.  Use
 * ...
 **/

public class TimeLimitProcess extends Process {

  private Process p;
  private long timeLimit;
  private boolean timed_out;
  // can make public for testing
  private /*@LazyNonNull*/ StringWriter cached_stdout;
  private /*@LazyNonNull*/ StringWriter cached_stderr;

  private Timer timer;

  private static boolean debug = false;

  /**
   * Creates a TimeLimitProcess with the given time limit, in wall clock
   * milliseconds.
   * Requires: p != null
   * @param timeLimit in milliseconds
   **/
  public TimeLimitProcess (Process p, long timeLimit) {
    this(p, timeLimit, false);
  }

  /**
   * Creates a TimeLimitProcess with the given time limit, in wall clock
   * milliseconds.
   * Requires: p != null
   * @param timeLimit in milliseconds
   * @param cacheStdout
   * If true, causes the TimeLimitProcess to consume the standard output of the
   * underlying process, and to cache it.  After the process terminates (on
   * its own or by being timed out), the output is available via the
   * cached_stdout method.  This is necessary because when a Java process
   * is terminated, its standard output is no longer available.
   */
  public TimeLimitProcess (Process p, long timeLimit, boolean cacheStdout) {
    this.p = p;
    timer = new Timer(true);
    this.timeLimit = timeLimit;
    if (debug) {
      System.out.printf("new timelimit process, timeLimit=%s, cacheStdout=%s%n",
                        timeLimit, cacheStdout);
    }
    timer.schedule(new TPTimerTask(this, timeLimit), timeLimit);
    if (cacheStdout) {
      cached_stdout = new StringWriter();
      cached_stderr = new StringWriter();
      new StdoutStreamReaderThread().start();
      new StderrStreamReaderThread().start();
    }
      
  }

  /**
   * Returns true if the process has timed out (has run for more than the
   * timeLimit msecs specified in the constructor).
   */
  public boolean timed_out() {
    return (timed_out);
  }

  /**
   * Returns the timeout time in msecs.
   */
  public long timeout_msecs() {
    return (timeLimit);
  }

  // /**
  //  * Returns the standard output of the process, if the cacheStdout
  //  * parameter was "true" when the constructor was invoked.
  //  * Only for debugging.
  //  */
  // public String cached_stdout() {
  //   if (cached_stdout == null) {
  //     throw new Error("called cached_stdout() without previously calling cache_stdout()");
  //   }
  //   return cached_stdout.toString();
  // }


  /**
   * Kills the subprocess.
   * @see Process#destroy()
   **/
  public void destroy() {
    p.destroy();
  }

  /**
   * Returns the exit value for the subprocess.
   * @see Process#getErrorStream()
   */
  public int exitValue() {
    // I'm not sure whether this is necessary; the Process.destroy()
    // documentation doesn't specify the effect on the exit value.
    if ((p.exitValue() == 0) && timed_out) {
      return 255;
    } else {
      return p.exitValue();
    }
  }

  /**
   * Gets the error stream of the subprocess.
   * @see Process#getErrorStream()
   */
  public InputStream getErrorStream() {
    if (cached_stderr == null) {
      return p.getErrorStream();
    } else {
      // Convert a String to an InputStream
      String text = cached_stderr.toString();
      try {
        InputStream is = new ByteArrayInputStream(text.getBytes("UTF-8"));
        return is;
      } catch (UnsupportedEncodingException e) {
        throw new Error(e);
      }
    }
  }

  /**
   * Gets the input stream of the subprocess.
   * @see Process#getInputStream()
   */
  public InputStream getInputStream() {
    if (cached_stdout == null) {
      return p.getInputStream();
    } else {
      return stringToInputStream(cached_stdout.toString());
    }
  }

  // Convert a String to an InputStream
  private InputStream stringToInputStream(String text) {
    try {
      InputStream is = new ByteArrayInputStream(text.getBytes("UTF-8"));
      return is;
    } catch (UnsupportedEncodingException e) {
      throw new Error(e);
    }
  }


  /**
   * Gets the output stream of the subprocess.
   * @see Process#getOutputStream()
   */
  public OutputStream getOutputStream() {
    return p.getOutputStream();
  }

  /**
   * Causes the current thread to wait, if necessary, until the process represented by this Process object has terminated.
   * @see Process#waitFor()
   */
  public int waitFor() throws InterruptedException {
    return p.waitFor();
  }

  /**
   * @return true if the process if finished, false otherwise
   **/
  public boolean finished () {
    try {
      // Process.exitValue() throws an exception if the process is not
      // finished.
      p.exitValue();
      return true;
    } catch (IllegalThreadStateException ie) {
      return false;
    }
  }

  /**
   * This TimerTask destroys the process that is passed to it.
   **/
  private static class TPTimerTask extends TimerTask {
    TimeLimitProcess tp;
    long timeLimit;
    public TPTimerTask(TimeLimitProcess tp, long timeLimit) {
      this.tp = tp;
      this.timeLimit = timeLimit;
    }
    public void run() {
      // If exitValue is queried while the process is still running,
      // the IllegalThreadStateException will be thrown.  If that
      // happens, we kill the process and note that so callers can
      // tell that a timeout occurred.
      try {
        int exit = tp.p.exitValue();
        if (debug) {
          System.out.println();
          System.out.println("Process exited with status " + exit);
          System.out.println();
        }
      } catch (IllegalThreadStateException ie) {
        tp.p.destroy();
        tp.timed_out = true;
        if (debug) {
          System.out.println("Terminated process after timelimit of "
                             + timeLimit + " msecs expired");
          System.out.println();
        }
      }
      this.cancel();
    }
  }

  // I'm not sure how to generalize the below two classes into one -- my
  // attempt failed.

  private class StdoutStreamReaderThread extends Thread {
    /*@NonNullOnEntry("cached_stdout")*/
    public void run() {
      // This thread will block as the process produces output.  That's OK,
      // because the blocking is happening in a separate thread.
      try {
        IOUtils.copy(p.getInputStream(), cached_stdout);
      } catch (IOException e) {
        // assume the best
      }
    }
  }

  private class StderrStreamReaderThread extends Thread {
    /*@NonNullOnEntry("cached_stderr")*/
    public void run() {
      // This thread will block as the process produces output.  That's OK,
      // because the blocking is happening in a separate thread.
      try {
        IOUtils.copy(p.getErrorStream(), cached_stderr);
      } catch (IOException e) {
        // assume the best
      }
    }
  }

}
