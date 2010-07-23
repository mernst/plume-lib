// The five files
//   Option.java
//   OptionGroup.java
//   Options.java
//   Unpublicized.java
//   OptionsDoclet.java
// together comprise the implementation of command-line processing.

package plume;

import java.io.*;
import java.util.*;
import com.sun.javadoc.*;

import java.lang.Class;

/**
 * Generates HTML documentation of command-line options.
 * This doclet is typically invoked with:
 * <pre>javadoc -quiet -doclet plume.OptionsDoclet [doclet options] [java files]</pre>
 * <p>
 *
 * The following doclet options are supported:
 * <ul>
 * <li> <b>-outfile</b> <i>file</i> The destination for the HTML
 * output (the default is standard out).  If both <code>-outfile</code> and
 * <code>-docfile</code> are specified, they must be different.
 *
 * <li> <b>-docfile</b> <i>file</i> When specified, the output of this doclet
 * is the result of replacing everything between the two lines
 * <pre>&lt;!-- start options doc (DO NOT EDIT BY HAND) --&gt;</pre>
 * and
 * <pre>&lt;!-- end options doc --&gt;</pre>
 * in <i>file</i> with the options documentation.  This can be used for
 * inserting option documentation into an existing manual.  The existing
 * docfile is not modified; output goes to the <code>-outfile</code>
 * argument, or to standard out.
 *
 * <li> <b>-classdoc</b> When specified, the output of this doclet includes the
 * class documentation of the first class specified on the command-line.
 * </ul>
 * <p>
 *
 * The generated HTML documentation includes unpublicized option groups but not
 * <code>@Unpublicized</code> options.  Option groups which contain only
 * <code>@Unpublicized</code> options are not included in the output at all.
 *
 * @see plume.Option
 * @see plume.Options
 * @see plume.OptionGroup
 * @see plume.Unpublicized
 */
// This doesn't itself use plume.Options for its command-line option
// processing because a Doclet is required to implement the optionLength
// and validOptions methods.
public class OptionsDoclet {

  @SuppressWarnings("nullness") // line.separator property always exists
  private static String eol = System.getProperty("line.separator");

  private static String startDelim = "<!-- start options doc (DO NOT EDIT BY HAND) -->";
  private static String endDelim = "<!-- end options doc -->"; 

  private static String usage = "Provided by Options doclet:\n" +
    "-docfile <file>        Specify file into which options documentation is inserted\n" +
    "-outfile <file>        Specify destination for resulting output\n" +
    "-classdoc              Include 'main' class documentation in output\n" +
    "-singledash            Use single dashes for long options (see plume.Options)\n" +
    "See the OptionsDoclet documentation for more details.";

  private File docFile;
  private File outFile;
  private boolean includeClassDoc = false;

  private Options options;

  /**
   * Entry point for the doclet.
   */
  public static boolean start(RootDoc root) {
    OptionsDoclet o = new OptionsDoclet();
    o.setOptions(root.options());

    List<Class<?>> classes = new ArrayList<Class<?>>();
    for (ClassDoc doc : root.specifiedClasses()) {
      // TODO: Class.forName() expects a binary name but doc.qualifiedName()
      // returns a fully qualified name.  I do not know a good way to convert
      // between these two name formats.  For now, we simply ignore inner
      // classes.  This limitation can be removed when we figure out a better
      // way to go from ClassDoc to Class<?>.
      if (doc.containingClass() != null)
        continue;
      try {
        classes.add(Class.forName(doc.qualifiedName(), true, Thread.currentThread().getContextClassLoader()));
      } catch (ClassNotFoundException e) {
        e.printStackTrace();
        return false;
      }
    }

    Object[] classarr = classes.toArray();
    o.options = new Options(classarr);
    if (o.options.getOptions().size() < 1) {
      System.out.println("Error: no @Option-annotated fields found");
      return false;
    }

    try {
      o.outputOptDoc(root);
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }

    return true;
  }

  /**
   * Returns the number of tokens corresponding to a command-line argument of
   * this doclet or 0 if the argument is unrecognized.  This method is
   * automatically invoked.
   *
   * @see <a href="http://java.sun.com/javase/6/docs/technotes/guides/javadoc/doclet/overview.html">Doclet overview</a>
   */
  public static int optionLength(String option) {
    if (option.equals("-help")) {
      System.out.println(usage);
      return 1;
    }
    if (option.equals("-classdoc") ||
        option.equals("-singledash")) {
      return 1;
    }
    if (option.equals("-docfile") ||
        option.equals("-outfile")) {
      return 2;
    }
    return 0;
  }

  /**
   * Tests the validity of command-line arguments passed to this doclet.
   * Returns true if the option usage is valid, and false otherwise.  This
   * method is automatically invoked.
   *
   * @see <a href="http://java.sun.com/javase/6/docs/technotes/guides/javadoc/doclet/overview.html">Doclet overview</a>
   */
  public static boolean validOptions(String options[][],
                                     DocErrorReporter reporter) {
    boolean hasDocFile = false;
    boolean hasOutFile = false;
    String docFile = null;
    String outFile = null;
    for (int oi = 0; oi < options.length; oi++) {
      String[] os = options[oi];
      String opt = os[0].toLowerCase();
      if (opt.equals("-docfile")) {
        if (hasDocFile) {
          reporter.printError("-docfile option specified twice");
          return false;
        }
        File f = new File(os[1]);
        if (!f.exists()) {
          reporter.printError("file not found: " + os[1]);
          return false;
        }
        docFile = os[1];
        hasDocFile = true;
      }
      if (opt.equals("-outfile")) {
        if (hasOutFile) {
          reporter.printError("-outfile option specified twice");
          return false;
        }
        outFile = os[1];
        hasOutFile = true;
      }
    }
    if (docFile != null && outFile != null && outFile.equals(docFile)) {
      reporter.printError("docfile must be different from outfile");
      return false;
    }
    return true;
  }

  /**
   * Set the options for this class based on command-line arguements given by
   * RootDoc.options().
   */
  private void setOptions(String[][] options) {
    for (int oi = 0; oi < options.length; oi++) {
      String[] os = options[oi];
      String opt = os[0].toLowerCase();
      if (opt.equals("-docfile")) {
        this.docFile = new File(os[1]);
      } else if (opt.equals("-outfile")) {
        this.outFile = new File(os[1]);
      } else if (opt.equals("-classdoc")) {
        this.includeClassDoc = true;
      } else if (opt.equals("-singledash")) {
        this.options.use_single_dash(true);
      }
    }
  }

  /**
   * Outputs the result of this doclet.
   */
  private void outputOptDoc(RootDoc root) throws Exception {
    String outHtml = generateHtml(root);

    PrintWriter out;
    if (outFile == null)
      out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(System.out)));
    else
      out = new PrintWriter(new BufferedWriter(new FileWriter(outFile)));

    if (docFile == null) {
      out.println(outHtml);
      out.flush();
      out.close();
      return;
    }

    BufferedReader doc = new BufferedReader(new FileReader(docFile));
    String docline;
    boolean replacing = false;
    boolean replaced_once = false;
    while ((docline = doc.readLine()) != null) {
      if (replacing) {
        if (docline.trim().equals(endDelim))
          replacing = false;
        else
          continue;
      }

      out.println(docline);

      if (!replaced_once && docline.trim().equals(startDelim)) {
        out.println(outHtml);
        replaced_once = true;
        replacing = true;
      }
    }

    out.flush();
    out.close();
    doc.close();
  }

  /**
   * Generates the string containing the HTML output for this instance.
   */
  private String generateHtml(RootDoc root) {
    StringBuilderDelimited b = new StringBuilderDelimited(eol);

    // Process each option and add in the javadoc info
    for (Options.OptionInfo oi : options.getOptions()) {
      ClassDoc opt_doc = root.classNamed(oi.get_declaring_class().getName());
      String nameWithUnderscores = oi.long_name.replace('-', '_');
      if (opt_doc != null) {
        for (FieldDoc fd : opt_doc.fields()) {
          if (fd.name().equals (nameWithUnderscores)) {
            // If Javadoc for field is unavailable, then use the @Option
            // description in the documentation.
            if (fd.getRawCommentText().length() == 0) {
              oi.jdoc = oi.description;
              break;
            }
            oi.jdoc = formatComment(fd);
            break;
          }
        }
      }
    }

    if (includeClassDoc) {
      b.append(formatComment(root.classes()[0]));
      b.append("<p>Command line options: </p>");
    }

    b.append("<ul>");
    if (!options.isUsingGroups()) {
      b.append(formatOptions(options.getOptions(), 2));
    } else {
      for (Options.OptionGroupInfo gi : options.getOptionGroups()) {
        // Do not include groups without publicized options in output
        if (!gi.containsPublicizedOption())
          continue;

        b.append("  <li>" + gi.name);
        b.append("    <ul>");
        b.append(formatOptions(gi.optionList, 6));
        b.append("    </ul>");
        b.append("  </li>");
      }
    }
    b.append("</ul>");

    return b.toString();
  }

  /**
   * Format a javadoc comment to HTML by wrapping the text of inline @link tags
   * and block @see tags in HTML 'code' tags.  This keeps most of the
   * information in the comment while still being presentable. <p>
   * 
   * This is only a temporary solution.  Ideally, @link/@see tags would be
   * converted to HTML links which point to actual documentation.
   */
  private String formatComment(Doc doc) {
    StringBuilder buf = new StringBuilder();
    Tag[] tags = doc.inlineTags();
    for (Tag tag : tags) {
      if (tag instanceof SeeTag)
        buf.append("<code>" + tag.text() + "</code>");
      else
        buf.append(tag.text());
    }
    SeeTag[] seetags = doc.seeTags();
    if (seetags.length > 0) {
      buf.append(" See: ");
      StringBuilderDelimited seebuf = new StringBuilderDelimited(", ");
      for (SeeTag tag : seetags)
        seebuf.append("<code>" + tag.text() + "</code>");
      buf.append(seebuf);
      buf.append(".");
    }
    return buf.toString();
  }

  /**
   * Format a list of options with HTML for use in generating the HTML
   * documentation.
   */
  private String formatOptions(List<Options.OptionInfo> opt_list, int indent) {
    StringBuilderDelimited buf = new StringBuilderDelimited(eol);
    for (Options.OptionInfo oi : opt_list) {
      if (oi.unpublicized)
        continue;
      String synopsis = htmlSynopsis(oi);
      buf.append(String.format("%" + indent + "s<li>%s</li>", "", synopsis));
    }
    return buf.toString();
  }

  /**
   * Construct the line of HTML describing an Option.
   */
  private String htmlSynopsis(Options.OptionInfo oi) {
    StringBuilder b = new StringBuilder();
    Formatter f = new Formatter(b);
    if (oi.short_name != null)
      f.format("<b>-%s</b> ", oi.short_name);
    for (String a : oi.aliases)
      f.format("<b>%s</b> ", a);
    String prefix = options.isUsingSingleDash() ? "-" : "--";
    f.format("<b>%s%s=</b><i>%s</i>. ", prefix, oi.long_name, oi.type_name);
    String default_str = "no default";
    if (oi.default_str != null)
      default_str = String.format("default %s", oi.default_str);
    f.format("%s [%s]", oi.jdoc, default_str);
    return b.toString();
  }
}
