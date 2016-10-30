package plume;

import static plume.EntryReader.Entry;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.regex.qual.*;
*/

/**
 * Lookup searches a set of files, much like <code>grep</code> does. However, Lookup searches by
 * entry (by default, paragraphs) rather than by line, respects comments (ignores matches within
 * them), respects <code>\include</code> directives (searches the named file), and has other
 * features.
 *
 * <p>Each search criterion is a keyword or regular expression. Lookup outputs each <em>entry</em>
 * that matches all the search criteria.
 *
 * <p>By default, search criteria are treated as keywords, and each paragraph is treated as an entry
 * &mdash; in other words, Lookup prints each paragraph (in any of the files) that contains all the
 * keywords, essentially performing paragraph-wise grep.
 *
 * <p>A file can contain one or more entries, each of which is a short entry or a long entry.
 *
 * <ul>
 *   <li>A short entry is a single paragraph (delimited from the next entry by a blank line). Lookup
 *       searches all of a short entry.
 *   <li>A long entry is introduced by a line that begins with '&gt;entry'. The remainder of that
 *       line is a one-line description of the entry. A long entry is terminated by '&lt;entry', by
 *       the start of a new long entry, or by the start of a new file. Lookup searches only the
 *       first line of a long entry.
 * </ul>
 *
 * If multiple entries match, the first line of each is printed. If only one entry matches, then
 * that entry is printed in its entirety.
 *
 * <p>By default, Lookup searches the file ~/lookup/root. Files can contain comments and can include
 * other files. Comments start with a % sign in the first column. Any comment line is ignored (it is
 * not treated as a blank line for the purpose of separating entries). A file can include another
 * file via a line of the form '\include{filename}'.
 *
 * <p>The default behavior can be customized by way of command-line options.
 *
 * <p>The <a name="command-line-options">command-line options</a> are as follows:
 * <!-- start options doc (DO NOT EDIT BY HAND) -->
 *
 * <ul>
 *   <li id="optiongroup:Getting-help">Getting help
 *       <ul>
 *         <li id="option:help"><b>-h</b> <b>--help=</b><i>boolean</i>. Show detailed help
 *             information and exit. [default false]
 *         <li id="option:verbose"><b>-v</b> <b>--verbose=</b><i>boolean</i>. Print progress
 *             information [default false]
 *       </ul>
 *
 *   <li id="optiongroup:Where-to-search">Where to search
 *       <ul>
 *         <li id="option:entry-file"><b>-f</b> <b>--entry-file=</b><i>string</i>. Specify the
 *             colon-separated search list for the file that contains information to be searched.
 *             Only the first file found is used, though it may itself contain include directives.
 *             [default ~/lookup/root]
 *         <li id="option:search-body"><b>-b</b> <b>--search-body=</b><i>boolean</i>. Search the
 *             body of long entries in addition to the entry's description. The bodies of short
 *             entries are always searched. [default false]
 *       </ul>
 *
 *   <li id="optiongroup:What-to-search-for">What to search for
 *       <ul>
 *         <li id="option:regular-expressions"><b>-e</b>
 *             <b>--regular-expressions=</b><i>boolean</i>. Specifies that keywords are regular
 *             expressions. If false, keywords are text matches. [default false]
 *         <li id="option:case-sensitive"><b>-c</b> <b>--case-sensitive=</b><i>boolean</i>. If true,
 *             keywords matching is case sensistive. By default, both regular expressions and text
 *             keywords are case-insensitive. [default false]
 *         <li id="option:word-match"><b>-w</b> <b>--word-match=</b><i>boolean</i>. If true, match a
 *             text keyword only as a separate word, not as a substring of a word. This option is
 *             ignored if regular_expressions is true. [default false]
 *       </ul>
 *
 *   <li id="optiongroup:How-to-print-matches">How to print matches
 *       <ul>
 *         <li id="option:print-all"><b>-a</b> <b>--print-all=</b><i>boolean</i>. By default, if
 *             multiple entries are matched, only a synopsis of each entry is printed. If
 *             'print_all' is selected then the body of each matching entry is printed. [default
 *             false]
 *         <li id="option:item-num"><b>-i</b> <b>--item-num=</b><i>integer</i>. Specifies which item
 *             to print when there are multiple matches. The index is 1-based; that is, it starts
 *             counting at 1.
 *         <li id="option:show-location"><b>-l</b> <b>--show-location=</b><i>boolean</i>. If true,
 *             show the filename/line number of each matching entry in the output. [default false]
 *       </ul>
 *
 *   <li id="optiongroup:Customizing-format-of-files-to-be-searched">Customizing format of files to
 *       be searched
 *       <ul>
 *         <li id="option:entry-start-re"><b>--entry-start-re=</b><i>regex</i>. Regex that denotes
 *             the start of a long entry [default ^&gt;entry *()]
 *         <li id="option:entry-stop-re"><b>--entry-stop-re=</b><i>regex</i>. Regex that denotes the
 *             end of a long entry [default ^&lt;entry]
 *         <li id="option:description-re"><b>--description-re=</b><i>regex</i>. Regex that finds an
 *             entry's description (for long entries)
 *         <li id="option:comment-re"><b>--comment-re=</b><i>string</i>. Regex that matches an
 *             entire comment (not just a comment start) [default ^%.*]
 *         <li id="option:include-re"><b>--include-re=</b><i>string</i>. Regex that matches an
 *             include directive; group 1 is the file name [default \\include\{(.*)\}]
 *       </ul>
 *
 * </ul>
 *
 * <!-- end options doc -->
 */
public final class Lookup {

  /** This class is a collection of methods; it does not represent anything. */
  private Lookup() {
    throw new Error("do not instantiate");
  }

  /** Show detailed help information and exit. */
  @OptionGroup("Getting help")
  @Option("-h Show detailed help information")
  public static boolean help = false;

  @Option("-v Print progress information")
  public static boolean verbose = false;

  // This uses only the first file because the default search path might be
  // something like user:system and you might want only your version of the
  // system files.  It might be useful to also support (via another flag,
  // or by taking over this one, or by the syntax of the separator, or in
  // some other way) specifying multiple files on the command line.
  /**
   * Specify the colon-separated search list for the file that contains information to be searched.
   * Only the first file found is used, though it may itself contain include directives.
   */
  @OptionGroup("Where to search")
  @Option(
      "-f Specify the colon-separated search list of files of information; may only be supplied once")
  public static String entry_file = "~/lookup/root";

  /**
   * Search the body of long entries in addition to the entry's description. The bodies of short
   * entries are always searched.
   */
  @Option("-b Search body of long entries for matches")
  public static boolean search_body = false;

  /** Specifies that keywords are regular expressions. If false, keywords are text matches. */
  @OptionGroup("What to search for")
  @Option("-e Keywords are regular expressions")
  public static boolean regular_expressions = false;

  /**
   * If true, keywords matching is case sensistive. By default, both regular expressions and text
   * keywords are case-insensitive.
   */
  @Option("-c Keywords are case sensistive")
  public static boolean case_sensitive = false;

  /**
   * If true, match a text keyword only as a separate word, not as a substring of a word. This
   * option is ignored if regular_expressions is true.
   */
  @Option("-w Only match text keywords against complete words")
  public static boolean word_match = false;

  /**
   * By default, if multiple entries are matched, only a synopsis of each entry is printed. If
   * 'print_all' is selected then the body of each matching entry is printed.
   */
  @OptionGroup("How to print matches")
  @Option("-a Print the entire entry for each match")
  public static boolean print_all = false;

  /**
   * Specifies which item to print when there are multiple matches. The index is 1-based; that is,
   * it starts counting at 1.
   */
  @Option("-i Choose a specific item when there are multiple matches; index is 1-based")
  public static /*@Nullable*/ Integer item_num;

  /** If true, show the filename/line number of each matching entry in the output. */
  @Option("-l Show the location of each matching entry")
  public static boolean show_location = false;

  @OptionGroup("Customizing format of files to be searched")
  @Option("Regex that denotes the start of a long entry")
  public static /*@Regex(1)*/ Pattern entry_start_re = Pattern.compile("^>entry *()");

  @Option("Regex that denotes the end of a long entry")
  public static Pattern entry_stop_re = Pattern.compile("^<entry");

  @Option("Regex that finds an entry's description (for long entries)")
  public static /*@Nullable*/ Pattern description_re = null;

  // If "", gets set to null immediately after option processing.
  @Option("Regex that matches an entire comment (not just a comment start)")
  public static /*@Nullable*/ /*@Regex*/ String comment_re = "^%.*";

  @Option("Regex that matches an include directive; group 1 is the file name")
  public static /*@Regex(1)*/ String include_re = "\\\\include\\{(.*)\\}";

  /** Platform-specific line separator. */
  private static final String lineSep = System.getProperty("line.separator");

  /** One line synopsis of usage. */
  private static String usage_string = "lookup [options] <keyword> ...";

  /**
   * Look for the specified keywords in the file(s) and print the corresponding entries.
   *
   * @param args command-line arguments; see documentation
   * @throws IOException if there is a problem reading a file
   */
  public static void main(String[] args) throws IOException {

    Options options = new Options(usage_string, Lookup.class);
    String[] keywords = options.parse_or_usage(args);

    // If help was requested, print it and exit
    if (help) {
      InputStream is = Lookup.class.getResourceAsStream("lookup.txt");
      if (is == null) {
        // This should never happen.
        System.out.println("Unable to find resource 'lookup.txt' with help text.");
        System.exit(1);
      }
      BufferedReader help_stream = new BufferedReader(new InputStreamReader(is));
      String line = help_stream.readLine();
      while (line != null) {
        System.out.println(line);
        line = help_stream.readLine();
      }
      System.exit(0);
    }

    if (verbose) {
      System.out.printf("Options settings: %n%s%n", options.settings());
    }

    // Make sure at least one keyword was specified
    if (keywords.length == 0) {
      options.print_usage("Error: No keywords specified");
      System.exit(254);
    }

    // comment_re starts out non-null and the option processing code can't
    // make it null, so no null pointer exception is possible in the
    // if statement predicate that immediately follows this assertion.
    assert comment_re != null : "@AssumeAssertion(nullness): application invariant";

    // If the comment regular expression is empty, turn off comment processing
    if (comment_re.equals("")) {
      comment_re = null;
    }

    // Open the first readable root file
    EntryReader reader = null;
    String[] entry_files = entry_file.split(":");
    List<Exception> file_errors = new ArrayList<Exception>();
    for (String ef : entry_files) {
      ef = UtilMDE.expandFilename(ef);
      try {
        reader = new EntryReader(ef, comment_re, include_re);
      } catch (FileNotFoundException e) {
        file_errors.add(e);
      }
      if (reader != null) {
        break;
      }
    }
    if (reader == null) {
      System.out.println("Error: Can't read any entry files");
      for (Exception file_error : file_errors) {
        System.out.printf("  entry file %s%n", file_error.getMessage());
      }
      System.exit(254);
    }

    // Setup the regular expressions for long entries
    reader.set_entry_start_stop(entry_start_re, entry_stop_re);

    List<Entry> matching_entries = new ArrayList<Entry>();

    try {
      // Process each entry looking for matches
      int entry_cnt = 0;
      Entry entry = reader.get_entry();
      while (entry != null) {
        entry_cnt++;
        if (verbose && ((entry_cnt % 1000) == 0)) {
          System.out.printf("%d matches in %d entries\r", matching_entries.size(), entry_cnt);
        }
        int matchcount = 0;
        for (String keyword : keywords) {
          String search = entry.get_description(description_re);
          if (search_body || entry.short_entry) {
            search = entry.body;
          }
          if (!case_sensitive) {
            search = search.toLowerCase();
          }
          if (regular_expressions) {
            int flags = Pattern.CASE_INSENSITIVE;
            if (case_sensitive) {
              flags = 0;
            }

            if (!RegexUtil.isRegex(keyword)) {
              System.out.println("Error: not a regex: " + keyword);
              System.exit(254);
            }

            if (Pattern.compile(keyword, flags).matcher(search).find()) {
              matchcount++;
            }
          } else {
            if (!case_sensitive) {
              keyword = keyword.toLowerCase();
            }
            if (word_match) {
              String keyword_re = "\\b" + Pattern.quote(keyword) + "\\b";
              if (Pattern.compile(keyword_re).matcher(search).find()) {
                matchcount++;
              }
            } else if (search.contains(keyword)) {
              matchcount++;
            }
          }
        }
        if (matchcount == keywords.length) {
          matching_entries.add(entry);
        }
        entry = reader.get_entry();
      }
    } catch (FileNotFoundException e) {
      System.out.printf(
          "Error: Can't read %s at line %d in file %s%n",
          e.getMessage(), reader.getLineNumber(), reader.getFileName());
      System.exit(254);
    }

    // Print the results
    if (matching_entries.size() == 0) {
      System.out.println("Nothing found.");
    } else if (matching_entries.size() == 1) {
      Entry e = matching_entries.get(0);
      if (show_location) {
        System.out.printf("%s:%d:%n", e.filename, e.line_number);
      }
      System.out.print(e.body);
    } else { // there must be multiple matches
      if (item_num != null) {
        if (item_num < 1) {
          System.out.printf("Illegal --item-num %d, should be positive%n", item_num);
          System.exit(1);
        }
        if (item_num > matching_entries.size()) {
          System.out.printf(
              "Illegal --item-num %d, should be <= %d%n", item_num, matching_entries.size());
          System.exit(1);
        }
        Entry e = matching_entries.get(item_num - 1);
        if (show_location) {
          System.out.printf("%s:%d:%n", e.filename, e.line_number);
        }
        System.out.print(e.body);
      } else {
        int i = 0;
        if (print_all) {
          System.out.printf(
              "%d matches found (separated by dashes below)%n", matching_entries.size());
        } else {
          System.out.printf(
              "%d matches found. Use -i to print a specific match or -a to see them all%n",
              matching_entries.size());
        }

        for (Entry e : matching_entries) {
          i++;
          if (print_all) {
            if (show_location) {
              System.out.printf("%n-------------------------%n%s:%d:%n", e.filename, e.line_number);
            } else {
              System.out.printf("%n-------------------------%n");
            }
            System.out.print(e.body);
          } else {
            if (show_location) {
              System.out.printf("  -i=%d %s:%d: %s%n", i, e.filename, e.line_number, e.first_line);
            } else {
              System.out.printf("  -i=%d %s%n", i, e.get_description(description_re));
            }
          }
        }
      }
    }
  }

  /**
   * Returns the next entry. If no more entries are available, returns null.
   *
   * @param reader where to read the entry from
   * @return the next entry, or null
   * @throws IOException if there is a problem reading a file
   */
  public static /*@Nullable*/ Entry old_get_entry(EntryReader reader) throws IOException {

    try {

      // Skip any preceeding blank lines
      String line = reader.readLine();
      while ((line != null) && (line.trim().length() == 0)) {
        line = reader.readLine();
      }
      if (line == null) {
        return (null);
      }

      Entry entry = null;
      String filename = reader.getFileName();
      long line_number = reader.getLineNumber();

      // If this is a long entry
      if (line.startsWith(">entry")) {

        // Get the current filename
        String current_filename = reader.getFileName();

        // Remove '>entry' from the line
        line = line.replaceFirst("^>entry *", "");
        String first_line = line;

        StringBuilder body = new StringBuilder();
        // Read until we find the termination of the entry
        while ((line != null)
            && !line.startsWith(">entry")
            && !line.equals("<entry")
            && current_filename.equals(reader.getFileName())) {
          body.append(line);
          body.append(lineSep);
          line = reader.readLine();
        }

        // If this entry was terminated by the start of the next one,
        // put that line back
        if ((line != null)
            && (line.startsWith(">entry") || !current_filename.equals(reader.getFileName()))) {
          reader.putback(line);
        }

        entry = new Entry(first_line, body.toString(), filename, line_number, false);

      } else { // blank separated entry

        String first_line = line;

        StringBuilder body = new StringBuilder();
        // Read until we find another blank line
        while ((line != null) && (line.trim().length() != 0)) {
          body.append(line);
          body.append(lineSep);
          line = reader.readLine();
        }

        entry = new Entry(first_line, body.toString(), filename, line_number, true);
      }

      return (entry);

    } catch (FileNotFoundException e) {
      System.out.printf(
          "Error: Can't read %s at line %d in file %s%n",
          e.getMessage(), reader.getLineNumber(), reader.getFileName());
      System.exit(254);
      return (null);
    }
  }

  /**
   * Returns the first line of entry.
   *
   * @param entry the entry whose first line to return
   * @return the first line of entry
   */
  public static String first_line(String entry) {

    int ii = entry.indexOf(lineSep);
    if (ii == -1) {
      return entry;
    }
    return entry.substring(0, ii);
  }
}
