package plume;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.regex.qual.*;
*/

/**
 * This class has method {@link #compileFiles(List)} that compiles Java source files. It invokes a
 * user-specified external command, such as <code>javac</code> or <code>jikes</code>.
 */
public final class FileCompiler {

  /** The Runtime of the JVM. */
  public static Runtime runtime = java.lang.Runtime.getRuntime();
  /** Matches the names of Java source files. Match group 1 is the complete filename. */
  static /*@Regex(1)*/ Pattern java_filename_pattern;
  /**
   * External command used to compile Java files, and command-line arguments. Guaranteed to be
   * non-empty.
   */
  private String[] compiler;
  /** Time limit for compilation jobs. */
  private long timeLimit;

  static {
    try {
      @SuppressWarnings(
          "regex") // output of escapeNonJava() can appear in a character class in a regex
      /*@Regex(1)*/ String java_filename_re
          // A javac error message may consist of several lines of output.
          // The filename will be found at the beginning of the first line,
          // the additional lines of information will all be indented.
          // (?m) turns on MULTILINE mode so the first "^" matches the
          // start of each error line output by javac. The blank space after
          // the second "^" is intentional; together with the first "^", this
          // says a filename can only be found at the start of a non-indented
          // line as noted above.
          = "(?m)^([^ ]+?\\.java)";
      java_filename_pattern = Pattern.compile(java_filename_re);
    } catch (PatternSyntaxException me) {
      me.printStackTrace();
      throw new Error("Error in regexp", me);
    }
  }

  /**
   * Creates a new FileCompiler. Equivalent to FileCompiler("javac", 6000).
   *
   * @see #FileCompiler(String, long)
   */
  public FileCompiler() {
    this("javac", 6000);
  }

  /**
   * Creates a new FileCompiler. Compared to {@link #FileCompiler(String,long)}, this constructor
   * permits spaces and other special characters in the command and arguments.
   *
   * @param compiler an array of Strings representing a command that runs a Java compiler (it could
   *     be the full path name or whatever is used on the commandline), plus any command-line
   *     options
   * @param timeLimit the maximum permitted compilation time, in msec
   */
  public FileCompiler(String[] compiler, long timeLimit) {
    if (compiler.length == 0) {
      throw new Error("no compile command was provided");
    }

    this.compiler = compiler;
    this.timeLimit = timeLimit;
  }

  /**
   * Creates a new FileCompiler. Compared to {@link #FileCompiler(String,long)}, this constructor
   * permits spaces and other special characters in the command and arguments.
   *
   * @param compiler a list of Strings representing a command that runs a Java compiler (it could be
   *     the full path name or whatever is used on the commandline), plus any command-line options
   * @param timeLimit the maximum permitted compilation time, in msec
   */
  public FileCompiler(ArrayList<String> compiler, long timeLimit) {
    this(compiler.toArray(new String[0]), timeLimit);
  }

  /**
   * Creates a new FileCompiler.
   *
   * @param compiler a command that runs a Java compiler; for instance, it could be the full path
   *     name or whatever is used on the commandline. It may contain command-line arguments, and is
   *     split on spaces.
   * @param timeLimit the maximum permitted compilation time, in msec
   */
  public FileCompiler(String compiler, long timeLimit) {
    this(compiler.trim().split(" +"), timeLimit);
  }

  /**
   * Compiles the files given by fileNames. Returns the error output.
   *
   * @return the error output from compiling the files
   * @param fileNames paths to the files to be compiled as Strings
   * @throws IOException if there is a problem reading a file
   */
  public String compileFiles(List<String> fileNames) throws IOException {

    // System.out.printf ("compileFiles: %s%n", fileNames);

    // Start a process to compile all of the files (in one command)
    TimeLimitProcess p = compile_source(fileNames);

    String compile_errors = "";
    String compile_output = "";

    try {
      int result = p.waitFor();
    } catch (Throwable e) {
      // Print stderr and stdout if there is an unexpected exception (timeout).
      compile_errors = UtilMDE.streamString(p.getErrorStream());
      compile_output = UtilMDE.streamString(p.getInputStream());
      System.out.println("Unexpected exception while compiling " + e);
      if (p.timed_out()) {
        System.out.println("Compile timed out after " + p.timeout_msecs() + " msecs");
      }
      // System.out.println ("Compile errors: " + compile_errors);
      // System.out.println ("Compile output: " + compile_output);
      e.printStackTrace();
      runtime.exit(1);
    }

    compile_errors = UtilMDE.streamString(p.getErrorStream());
    compile_output = UtilMDE.streamString(p.getInputStream());
    // System.out.println ("Compile errors: " + compile_errors);
    // System.out.println ("Compile output: " + compile_output);

    // javac tends to stop without completing the compilation if there
    // is an error in one of the files.  Remove all the erring files
    // and recompile only the good ones.
    if (compiler[0].indexOf("javac") != -1) {
      recompile_without_errors(fileNames, compile_errors);
    }

    return compile_errors;
  }

  //   /**
  //    * @param filename the path of the Java source to be compiled
  //    */
  //   private TimeLimitProcess compile_source(String filename) throws IOException {
  //     String command = compiler + " " + filename;
  //     // System.out.println ("\nexecuting compile command: " + command);
  //     return new TimeLimitProcess(runtime.exec(command), timeLimit, true);
  //   }

  /**
   * @param filenames the paths of the Java source to be compiled as Strings
   * @return the process that executed the external compile command
   * @throws Error if an empty list of filenames is provided
   */
  private TimeLimitProcess compile_source(List<String> filenames) throws IOException {
    int num_files = filenames.size();

    if (num_files == 0) {
      throw new Error("no files to compile were provided");
    }

    String[] command = new String[num_files + compiler.length];
    System.arraycopy(compiler, 0, command, 0, compiler.length);
    for (int i = 0; i < num_files; i++) {
      command[i + compiler.length] = filenames.get(i);
    }

    // System.out.println ("\nexecuting compile command: " + command);
    return new TimeLimitProcess(runtime.exec(command), timeLimit, true);
  }

  /**
   * Examine the errorString to identify the files that cannot compile, then recompile all the other
   * files. This function is necessary when compiling with javac because javac does not compile all
   * the files supplied to it if some of them contain errors. So some "good" files end up not being
   * compiled.
   *
   * @param fileNames all the files that were attempted to be compiled
   * @param errorString the error string that indicates which files could not be compiled
   */
  private void recompile_without_errors(List<String> fileNames, String errorString)
      throws IOException {
    // search the error string and extract the files with errors.
    if (errorString != null) {
      HashSet<String> errorClasses = new HashSet<String>();
      Matcher m = java_filename_pattern.matcher(errorString);
      while (m.find()) {
        @SuppressWarnings(
            "nullness") // Regex Checker imprecision:  find() guarantees that group 1 exists in regexp
        /*@NonNull*/ String sansExtension = m.group(1);
        errorClasses.add(sansExtension);
      }
      // Collect all the files that were not compiled into retry
      List<String> retry = new ArrayList<String>();
      String filenames = "";
      for (String sourceFileName : fileNames) {
        sourceFileName = sourceFileName.trim();
        String classFilePath = getClassFilePath(sourceFileName);
        if (!fileExists(classFilePath)) {
          if (!errorClasses.contains(sourceFileName)) {
            retry.add(sourceFileName);
            filenames += " " + sourceFileName;
          }
        }
      }

      if (retry.size() > 0) {
        TimeLimitProcess tp = compile_source(retry);

        try {
          tp.waitFor();
        } catch (InterruptedException e) {
          System.out.println("Compile of " + filenames + " interrupted: " + e);
        }
      }
    }
  }

  /**
   * Return the file path to where a class file for a source file at sourceFilePath would be
   * generated.
   *
   * @param sourceFilePath the path to the .java file
   * @return the path to the corresponding .class file
   */
  private static String getClassFilePath(String sourceFilePath) {
    int index = sourceFilePath.lastIndexOf('.');
    if (index == -1) {
      throw new IllegalArgumentException(
          "sourceFilePath: " + sourceFilePath + " must end with an extention.");
    }
    return sourceFilePath.substring(0, index) + ".class";
  }

  /**
   * Returns true if the given file exists.
   *
   * @param pathName path to check for existence
   * @return true iff the file exists
   */
  private static boolean fileExists(String pathName) {
    return (new File(pathName)).exists();
  }
}
