package plume;

import java.io.*;
import java.util.*;

/**
 * TimeLimitProcess is a subclass of Process such that the process is
 * killed if it runs for more than the specified number of milliseconds.
 * Wall clock seconds, not CPU seconds, are measured.
 * The process should already be started when TimeLimitProcess is invoked.
 * Typical use:
 * <pre>
 *   TimeLimitProcess p = new TimeLimitProcess(pb.start(), TIMEOUT_SEC * 1000);
 * </pre>
 *
 * PROBLEM: If the process is destroyed because it times out, then its
 * output is unreadable (Java code trying to read its output fails).  So,
 * either this code should busy-wait reading the standard and error outputs
 * and storing them away, or the client process should send its output to a
 * ByteArrayOutputStream or the like, which can be read after the process
 * terminates.
 **/

public class TimeLimitProcess extends Process {

  private Process p;
  private long timeLimit;
  private boolean timed_out;

  private Timer timer;

  /**
   * Creates a TimeLimitProcess with the given time limit, in wall clock
   * milliseconds.
   * Requires: p != null
   * @param timeLimit in milliseconds
   **/
  public TimeLimitProcess (Process p, long timeLimit) {
    this.p = p;
    timer = new Timer(true);
    this.timeLimit = timeLimit;
    // System.out.println ("new timelimit process, timeout = " + timeLimit);
    timer.schedule(new TPTimerTask(this), timeLimit);
    
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

  /**
   * Kills the subprocess.
   * @see Process.destroy()
   **/
  public void destroy() {
    p.destroy();
  }

  /**
   * Returns the exit value for the subprocess.
   * @see Process.getErrorStream()
   */
  public int exitValue() {
    return p.exitValue();
  }

  /**
   * Gets the error stream of the subprocess.
   * @see Process.getErrorStream()
   */
  public InputStream getErrorStream() {
    return p.getErrorStream();
  }

  /**
   * Gets the input stream of the subprocess.
   * @see Process.getInputStream()
   */
  public InputStream getInputStream() {
    return p.getInputStream();
  }

  /**
   * Gets the output stream of the subprocess.
   * @see Process.getOutputStream()
   */
  public OutputStream getOutputStream() {
    return p.getOutputStream();
  }

  /**
   * Causes the current thread to wait, if necessary, until the process represented by this Process object has terminated.
   * @see Process.waitFor()
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
    public TPTimerTask(TimeLimitProcess tp) {
      this.tp = tp;
    }
    public void run() {
      // If exitValue is queried while the process is still running,
      // the IllegalThreadStateException will be thrown.  If that
      // happens, we kill the process and note that so callers can
      // tell that a timeout occurred.
      try {
        int exit = tp.p.exitValue();
        // System.out.println();
        // System.out.println("Process exited with status " + exit);
        // System.out.println();
      } catch (IllegalThreadStateException ie) {
        tp.p.destroy();
        tp.timed_out = true;
        //System.out.println("Terminated process after timelimit of "
        //                    + timeLimit + " msecs expired");
        //System.out.println();
      }
      this.cancel();
    }
  }
}
