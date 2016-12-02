package plume;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/*>>>
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.regex.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * TaskManager extracts information about tasks from text files and provides structured output. For
 * example, it can extract all of the tasks associated with a specific milestone or person and total
 * the amount of work required.
 *
 * <p>The command-line arguments are as follows:
 * <!-- start options doc (DO NOT EDIT BY HAND) -->
 *
 * <ul>
 *   <li id="option:responsible"><b>-r</b> <b>--responsible=</b><i>string</i>. Include only those
 *       tasks assigned to the specified person
 *   <li id="option:milestone"><b>-m</b> <b>--milestone=</b><i>string</i>. Include only those tasks
 *       required for the specified milestone
 *   <li id="option:completed"><b>-c</b> <b>--completed=</b><i>boolean</i>. Include only completed
 *       tasks [default false]
 *   <li id="option:open"><b>-o</b> <b>--open=</b><i>boolean</i>. Include only open tasks [default
 *       false]
 *   <li id="option:verbose"><b>-v</b> <b>--verbose=</b><i>boolean</i>. Print progress information
 *       [default false]
 *   <li id="option:format"><b>-f</b> <b>--format=</b><i>enum</i>. Specify output format [default
 *       short_ascii]
 *       <ul>
 *         <li><b>short_ascii</b>
 *         <li><b>short_html</b>
 *         <li><b>milestone_html</b>
 *       </ul>
 *
 *   <li id="option:comment-re"><b>--comment-re=</b><i>string</i>. Regex that matches an entire
 *       comment (not just a comment start) [default ^%.*]
 *   <li id="option:include-re"><b>--include-re=</b><i>string</i>. Regex that matches an include
 *       directive; group 1 is the file name [default \\include\{(.*)\}]
 * </ul>
 *
 * <!-- end options doc -->
 */
public class TaskManager {

  /** The format in which to output the TaskManager information. */
  public enum OutputFormat {
    short_ascii,
    short_html,
    milestone_html
  };

  // Command line options
  @Option("-r Include only those tasks assigned to the specified person")
  public static /*@Nullable*/ String responsible = null;

  @Option("-m Include only those tasks required for the specified milestone")
  public static /*@Nullable*/ String milestone = null;

  @Option("-c Include only completed tasks")
  public static boolean completed = false;

  @Option("-o Include only open tasks")
  public static boolean open = false;

  @Option("-v Print progress information")
  public static boolean verbose = false;

  @Option("-f Specify output format")
  public static OutputFormat format = OutputFormat.short_ascii;

  @Option("Regex that matches an entire comment (not just a comment start)")
  public static /*@Regex*/ String comment_re = "^%.*";

  @Option("Regex that matches an include directive; group 1 is the file name")
  public static /*@Regex(1)*/ String include_re = "\\\\include\\{(.*)\\}";

  private static String usage_string = "TaskManger [options] <task-file> <task_file> ...";

  @SuppressWarnings("regex") // line.separator property is a legal regex
  public static final /*@Regex*/ String lineSep = System.getProperty("line.separator");

  /** Information about a single task. */
  public static class Task {

    String filename;
    long line_number;

    String task;
    String responsible;
    /*@Nullable*/ Date assigned_date;
    /*@Nullable*/ String milestone;
    Float duration;
    Float completed;
    /*@Nullable*/ String description;
    /*@Nullable*/ String notes;

    /*@EnsuresNonNull({"filename", "task", "responsible", "duration", "completed"})*/
    private void checkRep(
        /*>>> @UnderInitialization(Object.class) @Raw(Object.class) Task this*/) {
      assert filename != null
          : "No filename at line " + line_number + " @AssumeAssertion(nullness)";
      assert task != null : "No task at line " + line_number + " @AssumeAssertion(nullness)";
      assert responsible != null
          : "No responsible at line " + line_number + " @AssumeAssertion(nullness)";
      assert duration != null
          : "No duration at line " + line_number + " @AssumeAssertion(nullness)";
      assert completed != null
          : "No completed at line " + line_number + " @AssumeAssertion(nullness)";
    }

    public Task(String body, String filename, long line_number) throws IOException {

      this.filename = filename;
      this.line_number = line_number;

      String[] lines = body.split(lineSep);
      for (int ii = 0; ii < lines.length; ii++) {
        String line = lines[ii];

        // Get the item/value out of the record.  One line items
        // are specifed as '{item}: {value}'.  Multiple line items
        // have a start line of '{item}>' and an end line of '<{item}'
        // with any number of value lines between.
        /*@NonNull*/ String item;
        String value;
        if (line.matches("^[_a-zA-Z]+:.*")) {
          String[] sa = line.split(" *: *", 2);
          item = sa[0];
          value = sa[1];
          if (value.length() == 0) {
            value = null;
          }
        } else if (line.matches("^[-a-zA-Z]+>.*")) {
          item = line.replaceFirst(" *>.*", "");
          value = "";
          for (ii++; ii < lines.length; ii++) {
            String nline = lines[ii];
            if (nline.equals("<" + item)) {
              break;
            }
            value += nline + lineSep;
          }
        } else {
          throw new IOException("malformed line: " + line);
        }

        // parse the value based on the item and store it away
        if (item.equals("task")) {
          if (value == null) {
            throw new Error("Task with no value at line " + line_number);
          }
          task = value;
        } else if (item.equals("responsible")) {
          if (value == null) {
            responsible = "none";
          } else {
            responsible = value;
          }
        } else if (item.equals("assigned_date")) {
          if (value == null) {
            assigned_date = null;
          } else {
            DateFormat df = new SimpleDateFormat("yy-MM-dd");
            try {
              assigned_date = df.parse(value);
              assert assigned_date != null : value;
            } catch (Throwable t) {
              throw new RuntimeException(t);
            }
          }
        } else if (item.equals("milestone")) {
          if (value == null) {
            throw new Error("Milestone with no value at line " + line_number);
          }
          milestone = value;
        } else if (item.equals("duration")) {
          if (value == null) {
            // duration is often used without being checked against null
            throw new Error("Duration with no value at line " + line_number);
          }
          duration = Float.parseFloat(value);
        } else if (item.equals("completed")) {
          if (value == null) {
            throw new Error("Completed with no value at line " + line_number);
          }
          completed = Float.parseFloat(value);
        } else if (item.equals("description")) {
          if (value == null) {
            throw new Error("Description with no value at line " + line_number);
          }
          description = value;
        } else if (item.equals("notes")) {
          if (value == null) {
            throw new Error("Notes with no value at line " + line_number);
          }
          notes = value;
        } else {
          throw new IOException("unknown field " + item);
        }
      }
      // Check that all required fields are set.
      checkRep();
    }

    /*@SideEffectFree*/
    public static String short_str(float f) {
      if (((double) f) - Math.floor((double) (f)) > 0.1) {
        return String.format("%.1f", f);
      } else {
        return String.format("%d", Math.round(f));
      }
    }

    /*@SideEffectFree*/
    private String completion_str() {
      return String.format("%s/%s", short_str(completed), short_str(duration));
    }

    /*@SideEffectFree*/
    public String toString_short_ascii() {
      return String.format("%-10s %-10s %-6s %s", responsible, milestone, completion_str(), task);
    }

    /*@SideEffectFree*/
    public String toString_short_html(double total) {
      return String.format(
          "<tr> <td> %s </td><td> %s </td><td> %s </td><td> %f </td><td> %s </td></tr>",
          responsible, milestone, completion_str(), total, task);
    }

    /*@SideEffectFree*/
    public String toString_milestone_html(double total) {
      String resp_str = responsible;
      if (resp_str.equals("none")) {
        resp_str = "<font color=red><b>" + resp_str + "</b></font>";
      }
      return String.format(
          "<tr> <td> %s </td><td> %s </td><td> %.1f </td><td>"
              + "<a href=%s?file=%s&line=%d> %s </a></td></tr>",
          resp_str, completion_str(), total, "show_task_details.php", filename, line_number, task);
    }

    public String all_vals() {
      StringBuilder out = new StringBuilder();
      out.append("task:            " + task + lineSep);
      out.append("responsible:     " + responsible + lineSep);
      out.append("assigned_date:   " + assigned_date + lineSep);
      out.append("milestone:       " + milestone + lineSep);
      out.append("duration:        " + duration + lineSep);
      out.append("completed:       " + completed + lineSep);
      out.append("description:     " + description + lineSep);
      out.append("notes:           " + notes + lineSep);
      return out.toString();
    }
  }

  /** List of all of the tasks. */
  public List<Task> tasks = new ArrayList<Task>();

  /** empty TaskManger. */
  public TaskManager() {}

  /**
   * initializes a task manager with all of the tasks in filenames.
   *
   * @param filenames list of files to read tasks from
   * @throws IOException if there is trouble reading a file
   */
  public TaskManager(String[] filenames) throws IOException {

    // Read in each specified task file
    for (String filename : filenames) {
      filename = UtilMDE.expandFilename(filename);
      try (EntryReader reader = new EntryReader(filename, comment_re, include_re)) {
        while (true) {
          EntryReader.Entry entry = reader.get_entry();
          if (entry == null) {
            break;
          }
          try {
            tasks.add(new Task(entry.body, entry.filename, entry.line_number));
          } catch (IOException e) {
            throw new Error("Error parsing " + entry.filename + " at line " + entry.line_number, e);
          }
        }
      }
    }
  }

  public static void main(String[] args) throws IOException {

    Options options = new Options(usage_string, TaskManager.class);
    String[] filenames = options.parse_or_usage(args);

    if (verbose) {
      System.out.printf("Option settings: %s%n", options.settings());
    }

    // Make sure at least one file was specified
    if (filenames.length == 0) {
      options.print_usage("Error: No task files specified");
      System.exit(254);
    }

    TaskManager tm = new TaskManager(filenames);

    // Dump out the tasks
    if (verbose) {
      System.out.printf("All tasks:%n");
      for (Task task : tm.tasks) {
        System.out.printf("%s%n%n", task.all_vals());
      }
    }

    // Print specified tasks
    TaskManager matches = tm.responsible_match(responsible);
    matches = matches.milestone_match(milestone);
    if (open) {
      matches = matches.open_only();
    }
    if (completed) {
      matches = matches.completed_only();
    }
    switch (format) {
      case short_ascii:
        System.out.println(matches.toString_short_ascii());
        break;
      case short_html:
        System.out.println(matches.toString_short_html());
        break;
      case milestone_html:
        System.out.println(matches.toString_milestone_html());
        break;
    }
  }

  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public String toString_short_ascii() {
    StringBuilder out = new StringBuilder();
    for (Task task : tasks) {
      out.append(task.toString_short_ascii() + lineSep);
    }
    return (out.toString());
  }

  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public String toString_short_html() {
    StringBuilder out = new StringBuilder();
    double total = 0.0;
    String responsible = null;
    out.append("<table>" + lineSep);
    for (Task task : tasks) {
      if (!task.responsible.equals(responsible)) {
        responsible = task.responsible;
        total = 0.0;
      }
      total += (task.duration.floatValue() - task.completed.floatValue());
      out.append(task.toString_short_html(total) + lineSep);
    }
    out.append("</table>" + lineSep);
    return (out.toString());
  }

  @SuppressWarnings("purity") // side effect to local state (string creation)
  /*@SideEffectFree*/
  public String toString_milestone_html() {
    StringBuilder out = new StringBuilder();
    out.append("<table border cellspacing=0 cellpadding=2>" + lineSep);
    out.append("<tr> <th> Responsible <th> C/D <th> Total <th> Task </tr>" + lineSep);
    double total = 0.0;
    String responsible = null;
    for (Task task : tasks) {
      if (!task.responsible.equals(responsible)) {
        if (responsible != null) {
          out.append("<tr bgcolor=grey><td colspan=4></td></tr>" + lineSep);
        }
        responsible = task.responsible;
        total = 0.0;
      }
      total += (task.duration.floatValue() - task.completed.floatValue());
      out.append(task.toString_milestone_html(total) + lineSep);
    }
    out.append("</table>" + lineSep);
    return (out.toString());
  }

  /**
   * Adds the specified task to the end of the task list.
   *
   * @param task the task to be queued on the task list
   */
  public void add(Task task) {
    tasks.add(task);
  }

  /**
   * Create a new TaskManger with only those tasks assigned to responsible. All tasks match a
   * responsible value of null.
   *
   * @param responsible name of the responsible party, or null; search for tasks assigned to
   *     responsible
   * @return a TaskManger with only those tasks assigned to responsible
   */
  public TaskManager responsible_match(/*@Nullable*/ String responsible) {

    TaskManager tm = new TaskManager();

    for (Task task : tasks) {
      if ((responsible == null) || responsible.equalsIgnoreCase(task.responsible)) {
        tm.add(task);
      }
    }

    return tm;
  }

  /**
   * Create a new TaskManger with only those tasks in milestone.
   *
   * @param milestone milestone to search for
   * @return TaskManger with only the tasks in the given milestone
   */
  public TaskManager milestone_match(/*@Nullable*/ String milestone) {

    TaskManager tm = new TaskManager();
    if (milestone == null) {
      return tm;
    }

    for (Task task : tasks) {
      if (milestone.equalsIgnoreCase(task.milestone)) {
        tm.add(task);
      }
    }

    return tm;
  }

  /**
   * Create a new TaskManger with only completed tasks.
   *
   * @return a new TaskManger with only completed tasks
   */
  public TaskManager completed_only() {

    TaskManager tm = new TaskManager();

    for (Task task : tasks) {
      if (task.duration <= task.completed) {
        tm.add(task);
      }
    }

    return tm;
  }

  /**
   * Create a new TaskManger with only open tasks.
   *
   * @return a new TaskManger with only completed tasks
   */
  public TaskManager open_only() {

    TaskManager tm = new TaskManager();

    for (Task task : tasks) {
      if (task.duration > task.completed) {
        tm.add(task);
      }
    }

    return tm;
  }
}
