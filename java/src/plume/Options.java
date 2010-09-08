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
import java.util.regex.*;
import java.lang.reflect.*;
import java.lang.annotation.*;

/**
 * The Options class:
 * <ul>
 *   <li>parses command-line options and sets fields in your program accordingly,</li>
 *   <li>creates usage messages (such as printed by a <tt>--help</tt> option), and</li>
 *   <li>creates documentation suitable for a manual or manpage.</li>
 * </ul>
 * Thus, the programmer is freed from writing duplicative, boilerplate code
 * and documentation that could get out of sync with the rest of the program.
 * <p>
 * 
 * The programmer does not have to write any code, only declare and
 * document variables.  Each field that is annotated with @{@link
 * plume.Option} is automatically set from a command-line argument of the
 * same name.
 * <p>
 *
 * The main entry point is {@link #parse_or_usage(String[])}.
 * Typical use is:
 *
 * <!-- Example needs some more words of explanation and example command lines. -->
 * <!-- Given this code: --> <pre>
 *  public static class MyProgram {
 * 
 *    &#64;Option("-o &lt;filename&gt; the output file ")
 *    public static File outfile = new File("/tmp/foobar");
 * 
 *    &#64;Option("-i ignore case")
 *    public static boolean ignore_case;
 * 
 *    &#64;Option("set the initial temperature")
 *    public static double temperature = 75.0;
 * 
 *    public static void main(String[] args) {
 *      MyProgram myInstance = new MyProgram();
 *      Options options = new Options("MyProgram [options] infile outfile",
 *                                    myInstance, MyUtilityClass.class);
 *      String[] remaining_args = options.parse_or_usage(args);
 *      ...
 *    }
 *  }</pre>
 * A user may invoke the program using the command-line arguments
 * <tt>-o</tt>, <tt>--outfile</tt>, <tt>-i</tt>, <tt>--ignore-case</tt>,
 * and, <tt>--temperature</tt>. <p>
 *
 * The call to {@link #parse_or_usage} sets fields in object myInstance,
 * and sets static fields in class MyUtilityClass.  It returns the original
 * command line, with all options removed. <p>
 *
 * <b>@Option indicates a command-line option</b> <p>
 * The @{@link Option} annotation on a field specifies brief user documentation
 * and, optionally, a one-character short name that users may supply on the
 * command line.  The long name is taken from the name of the variable;
 * when the name contains an underscore, the user may substitute a hyphen
 * on the command line instead; for example, the --multi-word-variable
 * command-line option would set the variable multi_word_variable. <p>
 *
 * On the command line, the values for options are specified in the form
 * '--longname=value', '-shortname=value', '--longname value', or '-shortname
 * value'.  If {@link #use_single_dash(boolean)} is true, then the long names
 * take the form '-longname=value' or '-longname value'.  The value is
 * mandatory for all options except booleans.  Booleans are set to true if no
 * value is specified. <p>
 *
 * All arguments that start with '-' are processed as options.  To
 * terminate option processing at the first non-option argument, see {@link
 * #parse_options_after_arg(boolean)}.  Also, the special option '--'
 * terminates option processing; method <tt>parse_or_usage</tt> returns
 * all subsequent arguments (along with any preceding non-option arguments)
 * without scanning them for options. <p>
 *
 * A user may provide an option multiple times on the command line.  If the
 * field is a list, each entry is added to the list.  If the field is
 * not a list, then only the last occurrence is used (subsequent
 * occurrences overwrite the previous value). <p>
 *
 * <b>Unpublicized options</b> <p>
 * The @{@link Unpublicized} annotation causes an option not to be displayed
 * in the usage message.  This can be useful for options that are
 * preliminary, experimental, or for internal purposes only.  The @{@link
 * Unpublicized} annotation must be specified in addition to the @{@link
 * Option} annotation. <p>
 *
 * There are forms of the usage-message methods that can include even
 * unpublicized options; for example, see {@link #usage(boolean,String...)}.
 * <p>
 *
 * <b>Option groups</b> <p>
 * The @{@link OptionGroup} annotation can be used to assign a name to a set of
 * related options.  This is useful for organizing a list of
 * options.  Options in the same group are displayed under the same heading
 * in usage texts. <p> 
 *
 * The @{@link OptionGroup} annotation must be specified on a field in addition
 * to an @{@link Option} annotation.  The <code>@OptionGroup</code> annotation
 * acts like a delimiter &mdash; all <code>@Option</code>-annotated fields up to
 * the next <code>@OptionGroup</code> annotation belong to the same group.
 * When using option groups, the first <code>@Option</code>-annotated field of
 * every class and object passed to the {@link #Options(String, Object...)}
 * constructor must have an <code>@OptionGroup</code> annotation.  Furthermore,
 * the first parameter of an <code>@OptionGroup</code> annotation (the group
 * name) must be unique among all classes and objects passed to the {@link
 * #Options(String, Object...)} constructor. <p>
 *
 * If an option group itself is unpublicized:
 * <ul>
 *   <li>The default usage message omits the group and all options belonging
 *       to it.
 *   <li>An unpublicized option group (that has any publicized options) is
 *       included in documentation for a manual.
 *   <li>A field with an @{@link Unpublicized} annotation is excluded
 *       even when passing the group's name
 *       explicitly as a parameter to {@link #usage(String...)}.
 * </ul>
 * 
 * If an option group is not unpublicized but contains only unpublicized
 * options, it will not be included in the default usage message. <p>
 *
 * <b>Option aliases</b> <p>
 * The @{@link Option} annotation has an optional parameter <code>aliases</code>,
 * which accepts an array of strings.  Each string in the array is an alias for
 * the option being defined and can be used in place of an option's long name
 * or short name.  Aliases should start with a single dash or double dash.  It
 * is the user's responsibility to ensure that aliases does not cause ambiguity
 * and do not collide with other options. <p>
 *
 * For example:
 * <pre>
 *     // The user may supply --help, -h, or -help, all of which mean the same thing and set this variable
 *     &#64;Option(value="-h Print a help message", aliases={"-help"})
 *     public static boolean help;</pre>
 *
 * <b>Generating documentation for a manual or manpage</b> <p>
 * The class Javadoc for a class that has a main method should generally
 * contain a summary of all command-line options.  Such a summary can also
 * be useful in other circumstances.
 * See the {@link plume.OptionsDoclet} class for instructions about generating
 * HTML documentation. <p>
 * 
 * <b>Supported field types</b> <p>
 * A field with an @{@link Option} annotation may be of the following types:
 * <ul>
 *   <li>Primitive types:  boolean, int, long, float, double.
 *       (Primitives can also be represented as wrappers:  Boolean,
 *       Integer, Long, Float, Double.  Use of a wrapper type allows the
 *       argument to have no default value.)
 *   <li>Reference types that have a constructor with a single string
 *       parameter.
 *   <li>java.util.regex.Pattern.
 *   <li>enums.
 *   <li>Lists of any of the above reference types.
 * </ul> <p>
 *
 * <b>More examples</b> <p>
 *
 * Example clients of the Options library include {@link
 * plume.Lookup} and the <code>Main</code> class of
 * <a href="http://code.google.com/p/javarifier/">Javarifier</a>.  <p>
 *
 * <b>Limitations</b> <ul>
 *
 *  <li> Short options are only supported as separate entries
 *  (e.g., "-a -b") and not as a single group (e.g., "-ab").
 *
 *  <li> Not all primitive types are supported.
 *
 *  <li> Types without a constructor that takes a single <tt>String</tt>
 *  argument are not supported.
 *
 *  <li> The "--no-long" option to turn off a boolean option named "long"
 *  is not supported; use "--long=false" instead.
 *
 * </ul>
 *
 * <b>Possible enhancements</b> <ul>
 *  <li> Positional arguments (non-options that must be provided in a given
 *  order) could be supported.
 * </ul>
 *
 * @see plume.Option
 * @see plume.OptionGroup
 * @see plume.Unpublicized
 * @see plume.OptionsDoclet
 **/
public class Options {
  
  @SuppressWarnings("nullness") // line.separator property always exists
  private static String eol = System.getProperty("line.separator");

  /** Information about an option **/
  class OptionInfo {

    /** Field containing the value of the option **/
    Field field;

    /** Option information for the field **/
    Option option;

    /** Object containing the field.  Null if the field is static. **/
    /*@Nullable*/ Object obj;

    /** Short (one character) argument name **/
    /*@Nullable*/ String short_name;

    /** Long argument name **/
    String long_name;

    /** Aliases for this option **/
    String[] aliases;

    /** Argument description **/
    String description;

    /** Javadoc description **/
    /*@Nullable*/ String jdoc;

    /**
     * Name of the argument type.  Defaults to the type of the field, but
     * user can override this in the option string.
     */
    String type_name;

    /**
     * Class type of this field.  If the field is a list, the basetype
     * of the list.
     */
    Class<?> base_type;

    /** Default value of the option as a string **/
    /*@Nullable*/ String default_str = null;

    /**
     * If true, the default value string for this option will be excluded from
     * OptionsDoclet documentation.
     */
    boolean no_doc_default = false;

    /** If the option is a list, this references that list. **/
    /*@LazyNonNull*/ List<Object> list = null;

    /** Constructor that takes one String for the type **/
    /*@Nullable*/ Constructor<?> constructor = null;

    /** Factory that takes a string (some classes don't have a string constructor) and always returns non-null. */
    /*@Nullable*/ Method factory = null;

    /**
     * If true, this OptionInfo is not output when printing documentation.
     * @see #usage()
     */
    boolean unpublicized;

    /**
     * Create the specified option.  If obj is null, the field must be
     * static.  The short name, type name, and description are taken
     * from the option annotation.  The long name is the name of the
     * field.  The default value is the current value of the field.
     */
    OptionInfo (Field field, Option option, /*@Nullable*/ Object obj, boolean unpublicized) {
      this.field = field;
      this.option = option;
      this.obj = obj;
      this.base_type = field.getType();
      this.unpublicized = unpublicized;
      this.aliases = option.aliases();
      this.no_doc_default = option.noDocDefault();

      // The long name is the name of the field
      long_name = field.getName();
      if (use_dashes)
        long_name = long_name.replace ('_', '-');

      // Get the default value (if any)
      Object default_obj = null;
      if (!Modifier.isPublic (field.getModifiers()))
        throw new Error ("option field is not public: " + field);
      try {
        default_obj = field.get (obj);
        if (default_obj != null)
          default_str = default_obj.toString();
      } catch (Exception e) {
        throw new Error ("Unexpected error getting default for " + field, e);
      }

      // Handle lists.  When a list argument is specified multiple times,
      // each argument value is appended to the list.
      Type gen_type = field.getGenericType();
      if (gen_type instanceof ParameterizedType) {
        ParameterizedType pt = (ParameterizedType) gen_type;
        Type raw_type = pt.getRawType();
        if (!raw_type.equals (List.class))
          throw new Error ("@Option does not support type " + pt + " for field " + field);
        if (default_obj == null) {
          List<Object> new_list = new ArrayList<Object>();
          try {
            field.set(obj, new_list);
          } catch (Exception e) {
            throw new Error ("Unexpected error setting default for " + field, e);
          }
          default_obj = new_list;
        }
        if (((List<?>) default_obj).isEmpty())
          default_str = null;
        @SuppressWarnings("unchecked")
        List<Object> default_obj_as_list = (List<Object>) default_obj;
        this.list = default_obj_as_list;
        // System.out.printf ("list default = %s%n", list);
        this.base_type = (Class<?>) pt.getActualTypeArguments()[0];

        // System.out.printf ("Param type for %s = %s%n", field, pt);
        // System.out.printf ("raw type = %s, type = %s%n", pt.getRawType(),
        //                   pt.getActualTypeArguments()[0]);
      }

      // Get the short name, type name, and description from the annotation
      ParseResult pr = parse_option (option.value());
      short_name = pr.short_name;
      if (pr.type_name != null) {
        type_name = pr.type_name;
      } else {
        type_name = type_short_name (base_type);
      }
      description = pr.description;

      // Get a constructor for non-primitive base types
      if (!base_type.isPrimitive() && !base_type.isEnum()) {
        try {
          if (base_type == Pattern.class) {
            factory = Pattern.class.getMethod ("compile", String.class);
          } else { // look for a string constructor
            assert base_type != null; // nullness checker: problem with flow
            constructor = base_type.getConstructor (String.class);
          }
        } catch (Exception e) {
          throw new Error ("Option " + field
                           + " does not have a string constructor", e);
        }
      }
    }

    /**
     * Returns whether or not this option has a required argument.
     */
    public boolean argument_required() {
      Class<?> type = field.getType();
      return ((type != Boolean.TYPE) && (type != Boolean.class));
    }

    /**
     * Returns a short synopsis of the option in the form
     * -s --long=<type>
     * <strong>or</strong>
     * -s -long=<type>
     * if use_single_dash is true.
     **/
    public String synopsis() {
      String prefix = use_single_dash ? "-" : "--";
      String name = prefix + long_name;
      if (short_name != null)
        name = String.format ("-%s %s", short_name, name);
      name += String.format ("=<%s>", type_name);
      if (list != null)
        name += " [+]";
      return (name);
    }

    /**
     * Returns a one-line description of the option.
     */
    public String toString() {
      String prefix = use_single_dash ? "-" : "--";
      String short_name_str = "";
      if (short_name != null)
        short_name_str = "-" + short_name + " ";
      return String.format ("%s%s%s field %s", short_name_str, prefix,
                            long_name, field);
    }

    /** Returns the class that declares this option. **/
    public Class<?> get_declaring_class() {
      return field.getDeclaringClass();
    }
  }

  /** Information about an option group **/
  class OptionGroupInfo {

    /** The name of this option group **/
    String name;

    /**
     * If true, this group of options will not be printed in usage output by
     * default. However, the usage information for this option group can be
     * printed by specifying the group explicitly in the call to {@link
     * #usage}.
     */
    boolean unpublicized;

    /** List of options that belong to this group **/
    List<OptionInfo> optionList;

    OptionGroupInfo(String name, boolean unpublicized) {
      optionList = new ArrayList<OptionInfo>();
      this.name = name;
      this.unpublicized = unpublicized;
    }

    OptionGroupInfo(OptionGroup optionGroup) {
      optionList = new ArrayList<OptionInfo>();
      this.name = optionGroup.value();
      this.unpublicized = optionGroup.unpublicized();
    }

    /**
     * If false, this group of options does not contain any publicized options,
     * so it will not be included in the default usage message.
     */
    boolean any_publicized() {
      for (OptionInfo oi : optionList)
        if (!oi.unpublicized)
          return true;
      return false;
    }
  }


  /**
   * Whether to parse options after a non-option command-line argument.
   * @see #parse_options_after_arg(boolean)
   **/
  private boolean parse_options_after_arg = true;

  /** All of the argument options as a single string **/
  private String options_str = "";

  /** First specified class.  Void stands for "not yet initialized". **/
  private Class<?> main_class = Void.TYPE;

  /** List of all of the defined options **/
  private List<OptionInfo> options = new ArrayList<OptionInfo>();

  /** Map from short or long option names (with leading dashes) to option information **/
  private Map<String,OptionInfo> name_map
    = new LinkedHashMap<String,OptionInfo>();

  /** Map from option group name to option group information **/
  private Map<String, OptionGroupInfo> group_map
    = new LinkedHashMap<String, OptionGroupInfo>();

  /**
   * If, after the Options constructor is called, use_groups is true, then the
   * user is using @OptionGroup annotations correctly (as per the requirement
   * specified above).  If false, then @OptionGroup annotations have not been
   * specified on any @Option-annotated fields.  When @OptionGroup annotations
   * are used incorrectly, an Error is thrown by the Options constructor.
   */
  private boolean use_groups;

  /**
   * Convert underscores to dashes in long options in usage messages.  Users
   * may specify either the underscore or dashed name on the command line.
   */
  private boolean use_dashes = true;

  /**
   * When true, long options take the form -longOption with a single dash,
   * rather than the default --longOption with two dashes.
   */
  private boolean use_single_dash = false;

  /**
   * String describing "[+]" (copied from Mercurial).
   */
  private static String list_help = "[+] marked option can be specified multiple times";

  /**
   * Whether printing the usage message should print list_help.  The default is
   * to print list_help if the usage message contains an option that accepts a
   * list as a parameter.
   */
  private boolean print_list_help = false;

  /**
   * When true, an argument to a option of list type is split, on
   * whitespace, into multiple arguments each of which is added to the
   * list.  When false, each argument to an option of list type is treated
   * as a single element, no matter what characters it contains.
   */
  @Option ("Treat arguments to lists as space-separated.")
  public static boolean split_lists = false;

  /**
   * Synopsis of usage.  Example:  "prog [options] arg1 arg2 ..."
   * <p>
   * This variable is public so that clients can reset it (useful for
   * masquerading as another program, based on parsed options).
   **/
  public /*@Nullable*/ String usage_synopsis = null;

  // Debug loggers
  private SimpleLog debug_options = new SimpleLog (false);

  /**
   * Prepare for option processing.  Creates an object that will set fields
   * in all the given arguments.  An argument to this method may be a
   * Class, in which case its static fields are set.  The names of all the
   * options (that is, the fields annotated with &#064;{@link Option}) must be
   * unique across all the arguments.
   */
  public Options (Object... args) {
    this ("", args);
  }

  /**
   * Prepare for option processing.  Creates an object that will set fields
   * in all the given arguments.  An argument to this method may be a
   * Class, in which case its static fields are set.  The names of all the
   * options (that is, the fields annotated with &#064;{@link Option}) must be
   * unique across all the arguments.
   * @param usage_synopsis A synopsis of how to call your program
   */
  public Options (String usage_synopsis, Object... args) {

    if (args.length == 0) {
      throw new Error("Must pass at least one object to Options constructor");
    }

    this.usage_synopsis = usage_synopsis;
    
    this.use_groups = false;

    // true once the first @Option annotation is observed, false until then.
    boolean seen_first_opt = false;

    // Loop through each specified object or class
    for (Object obj : args) {
      boolean is_class = obj instanceof Class<?>;
      String current_group = null;

      Field[] fields;
      if (is_class) {
        if (main_class == Void.TYPE)
          main_class = (Class<?>) obj;
        fields = ((Class<?>) obj).getDeclaredFields();
      } else {
        if (main_class == Void.TYPE)
          main_class = obj.getClass();
        fields = obj.getClass().getDeclaredFields();
      }

      for (Field f : fields) {
        debug_options.log ("Considering field %s of object %s with annotations %s%n",
                           f, obj, Arrays.toString(f.getDeclaredAnnotations()));
        Option option = safeGetAnnotation(f, Option.class);
        if (option == null)
          continue;

        boolean unpublicized = safeGetAnnotation(f, Unpublicized.class) != null;

        if (is_class && !Modifier.isStatic (f.getModifiers()))
          throw new Error ("non-static option " + f + " in class " + obj);

        OptionInfo oi = new OptionInfo(f, option, is_class ? null : obj, unpublicized);
        options.add(oi);

        // FIXME: should also check that the option does not belong to an
        // unpublicized option group
        if (oi.list != null && !oi.unpublicized)
          print_list_help = true;

        OptionGroup optionGroup = safeGetAnnotation(f, OptionGroup.class);

        if (!seen_first_opt) {
          seen_first_opt = true;
          // This is the first @Option annotation encountered so we can decide
          // now if the user intends to use option groups.
          if (optionGroup != null)
            use_groups = true;
          else
            continue;
        }

        if (!use_groups) {
          if (optionGroup != null)
            // The user included an @OptionGroup annotation in their code
            // without including an @OptionGroup annotation on the first
            // @Option-annotated field, hence violating the requirement.

            // NOTE: changing this error string requires changes to TestPlume
            throw new Error("missing @OptionGroup annotation on the first " +
                            "@Option-annotated field of class " + main_class);
          else
            continue;
        }

        // use_groups is true at this point.  The variable current_group is set
        // to null at the start of every iteration through 'args'.  This is so
        // we can check that the first @Option-annotated field of every
        // class/object in 'args' has an @OptionGroup annotation when use_groups
        // is true, as required.
        if (current_group == null && optionGroup == null) {
          // NOTE: changing this error string requires changes to TestPlume
          throw new Error("missing @OptionGroup annotation in field "
                          + f + " of class " + obj);
        } else if (optionGroup != null) {
          String name = optionGroup.value();
          if (group_map.containsKey(name))
              throw new Error("option group " + name + " declared twice");
          OptionGroupInfo gi = new OptionGroupInfo(optionGroup);
          group_map.put(name, gi);
          current_group = name;
        } // current_group is non-null at this point
        @SuppressWarnings("nullness") // map key
        /*@NonNull*/ OptionGroupInfo ogi = group_map.get(current_group);
        ogi.optionList.add(oi);

      } // loop through fields
    } // loop through args

    String prefix = use_single_dash ? "-" : "--";

    // Add each option to the option name map
    for (OptionInfo oi : options) {
      if (oi.short_name != null) {
        if (name_map.containsKey ("-" + oi.short_name))
          throw new Error ("short name " + oi + " appears twice");
        name_map.put ("-" + oi.short_name, oi);
      }
      if (name_map.containsKey (prefix + oi.long_name))
        throw new Error ("long name " + oi + " appears twice");
      name_map.put (prefix + oi.long_name, oi);
      if (use_dashes && oi.long_name.contains ("-"))
        name_map.put (prefix + oi.long_name.replace ('-', '_'), oi);
      if (oi.aliases.length > 0) {
        for (String alias : oi.aliases) {
          if (name_map.containsKey (alias))
            throw new Error ("alias " + oi + " appears twice");
          name_map.put (alias, oi);
        }
      }
    }
  }

  /**
   * Like getAnnotation, but returns null (and prints a warning) rather
   * than throwing an exception.
   */
  private <T extends Annotation> /*@Nullable*/ T
  safeGetAnnotation(Field f, Class<T> annotationClass) {
    /*@Nullable*/ T annotation;
    try {
      annotation = f.getAnnotation(annotationClass);
    } catch (Exception e) {
      // Can get
      //   java.lang.ArrayStoreException: sun.reflect.annotation.TypeNotPresentExceptionProxy
      // when an annotation is not present at run time (example: @NonNull)
      System.out.printf("Exception in call to f.getAnnotation(%s)%n  for f=%s%n"
                        + "  %s%nClasspath =%n", annotationClass, f, e.getMessage());
      //e.printStackTrace();
      JWhich.printClasspath();
      annotation = null;
    }

    return annotation;
  }

  /**
   * If true, Options will parse arguments even after a non-option
   * command-line argument.  Setting this to true is useful to permit users
   * to write options at the end of a command line.  Setting this to false
   * is useful to avoid processing arguments that are actually
   * options/arguments for another program that this one will invoke.
   */
  public void parse_options_after_arg (boolean val) {
    parse_options_after_arg = val;
  }

  /** @deprecated Use {@link #parse_options_after_arg(boolean)}. */
  @Deprecated
  public void ignore_options_after_arg (boolean val) {
    parse_options_after_arg = !val;
  }

  /**
   * If true, long options (those derived from field names) will be parsed with
   * a single dash prefix as in -longOption.  The default is false and long
   * options will be parsed with a double dash prefix as in --longOption.
   */
  public void use_single_dash (boolean val) {
    use_single_dash = val;
  }

  /**
   * Parses a command line and sets the options accordingly.
   * @return all non-option arguments
   * @throws ArgException if the command line contains unknown option or
   * misused options.
   */
  public String[] parse (String[] args) throws ArgException {

    List<String> non_options = new ArrayList<String>();
    // If true, then "--" has been seen and any argument starting with "-"
    // is processed as an ordinary argument, not as an option.
    boolean ignore_options = false;

    // Loop through each argument
    for (int ii = 0; ii < args.length; ii++) {
      String arg = args[ii];
      if (arg.equals ("--")) {
        ignore_options = true;
      } else if ((arg.startsWith ("--") || arg.startsWith("-")) && !ignore_options) {
        String arg_name;
        String arg_value;
        int eq_pos = arg.indexOf ('=');
        if (eq_pos == -1) {
          arg_name = arg;
          arg_value = null;
        } else {
          arg_name = arg.substring (0, eq_pos);
          arg_value = arg.substring (eq_pos+1);
        }
        OptionInfo oi = name_map.get (arg_name);
        if (oi == null) {
          StringBuilder msg = new StringBuilder();
          msg.append(String.format("unknown option name '%s' in arg '%s'",
                                   arg_name, arg));
          if (false) { // for debugging
            msg.append("; known options:");
            for (String option_name : UtilMDE.sortedKeySet(name_map)) {
              msg.append(" ");
              msg.append(option_name);
            }
          }
          throw new ArgException (msg.toString());
        }
        if (oi.argument_required() && (arg_value == null)) {
          ii++;
          if (ii >= args.length)
            throw new ArgException ("option %s requires an argument", arg);
          arg_value = args[ii];
        }
        // System.out.printf ("arg_name = '%s', arg_value='%s'%n", arg_name,
        //                    arg_value);
        set_arg (oi, arg_name, arg_value);
      } else { // not an option
        if (! parse_options_after_arg)
          ignore_options = true;
        non_options.add (arg);
      }

    }
    String[] result = non_options.toArray (new String[non_options.size()]);
    return result;
  }

  /**
   * Parses a command line and sets the options accordingly.  This method
   * splits the argument string into command-line arguments, respecting
   * single and double quotes, then calls {@link #parse(String[])}.
   * <p>
   * {@link #parse(String[])} is usually a better method to call.  This one
   * is appropriate when the <tt>String[]</tt> version of the arguments is
   * not available &mdash; for example, for the <tt>premain</tt> method of
   * a Java agent.
   *
   * @return all non-option arguments
   * @throws ArgException if the command line contains misused options or an unknown option.
   * @see #parse(String[])
   */
  public String[] parse (String args) throws ArgException {

    // Split the args string on whitespace boundaries accounting for quoted
    // strings.
    args = args.trim();
    List<String> arg_list = new ArrayList<String>();
    String arg = "";
    char active_quote = 0;
    for (int ii = 0; ii < args.length(); ii++) {
      char ch = args.charAt (ii);
      if ((ch == '\'') || (ch == '"')) {
        arg+= ch;
        ii++;
        while ((ii < args.length()) && (args.charAt(ii) != ch))
          arg += args.charAt(ii++);
        arg += ch;
      } else if (Character.isWhitespace (ch)) {
        // System.out.printf ("adding argument '%s'%n", arg);
        arg_list.add (arg);
        arg = "";
        while ((ii < args.length()) && Character.isWhitespace(args.charAt(ii)))
          ii++;
        if (ii < args.length())
          ii--;
      } else { // must be part of current argument
        arg += ch;
      }
    }
    if (!arg.equals (""))
      arg_list.add (arg);

    String[] argsArray = arg_list.toArray (new String[arg_list.size()]);
    return parse (argsArray);
  }

  /**
   * Parses a command line and sets the options accordingly.  If an error
   * occurs, prints the usage message and terminates the program.  The program is
   * terminated rather than throwing an error to create cleaner output.
   * @return all non-option arguments
   * @see #parse(String[])
   */
  public String[] parse_or_usage (String[] args) {

    String non_options[] = null;

    try {
      non_options = parse (args);
    } catch (ArgException ae) {
      String message = ae.getMessage();
      if (message != null) {
        print_usage (message);
      } else {
        print_usage ();
      }
      System.exit (-1);
      // throw new Error ("usage error: ", ae);
    }
    return (non_options);
  }

  /**
   * Parses a command line and sets the options accordingly.  If an error
   * occurs, prints the usage message and terminates the program.  The program is
   * terminated rather than throwing an error to create cleaner output.
   * <p>
   * This method splits the argument string into command-line arguments,
   * respecting single and double quotes, then calls
   * {@link #parse_or_usage(String[])}.
   * <p>
   * {@link #parse(String[])} is usually a better method to call.  This one
   * is appropriate when the <tt>String[]</tt> version of the arguments is
   * not available &mdash; for example, for the <tt>premain</tt> method of
   * a Java agent.
   *
   * @return all non-option arguments
   * @see #parse_or_usage(String[])
   */
  public String[] parse_or_usage (String args) {

    String non_options[] = null;

    try {
      non_options = parse (args);
    } catch (ArgException ae) {
      String message = ae.getMessage();
      if (message != null) {
        print_usage (message);
      } else {
        print_usage ();
      }
      System.exit (-1);
      // throw new Error ("usage error: ", ae);
    }
    return (non_options);
  }

  /** @deprecated Use {@link #parse_or_usage(String[])}. */
  @Deprecated
  public String[] parse_and_usage (String[] args) {
    return parse_or_usage(args);
  }

  /** @deprecated Use {@link #parse_or_usage(String)}. */
  @Deprecated
  public String[] parse_and_usage (String args) {
    return parse_or_usage(args);
  }


  /// This is a lot of methods, but it does save a tad of typing for the
  /// programmer.

  /**
   * Prints usage information.  Uses the usage synopsis passed into the
   * constructor, if any.
   */
  public void print_usage (PrintStream ps) {
    if (usage_synopsis != null) {
      ps.printf ("Usage: %s%n", usage_synopsis);
    }
    ps.println(usage());
    if (print_list_help) {
      ps.println();
      ps.println(list_help);
    }
  }

  /**
   * Prints, to standard output, usage information.
   */
  public void print_usage () {
    print_usage (System.out);
  }

  // This method is distinct from
  //   print_usage (PrintStream ps, String format, Object... args)
  // because % characters in the message are not interpreted.
  /**
   * Prints a message followed by indented usage information.
   * The message is printed in addition to (not replacing) the usage synopsis.
   **/
  public void print_usage (PrintStream ps, String msg) {
    ps.println (msg);
    print_usage (ps);
  }

  /**
   * Prints, to standard output, a message followed by usage information.
   * The message is printed in addition to (not replacing) the usage synopsis.
   **/
  public void print_usage (String msg) {
    print_usage (System.out, msg);
  }

  /**
   * Prints a message followed by usage information.
   * The message is printed in addition to (not replacing) the usage synopsis.
   */
  public void print_usage (PrintStream ps, String format, /*@Nullable*/ Object... args) {
    ps.printf (format, args);
    if (! format.endsWith("%n")) {
      ps.println();
    }
    print_usage (ps);
  }

  /**
   * Prints, to standard output, a message followed by usage information.
   * The message is printed in addition to (not replacing) the usage synopsis.
   */
  public void print_usage (String format, /*@Nullable*/ Object... args) {
    print_usage(System.out, format, args);
  }

  /**
   * Returns the String containing the usage message for command-line options.
   *
   * @param group_names The list of option groups to include in the usage
   * message.  If empty and option groups are being used, will return usage
   * for all option groups that are not unpublicized.  If empty and option
   * groups are not being used, will return usage for all options that are
   * not unpublicized.
   */
  public String usage(String... group_names) {
    return usage(false, group_names);
  }

  /**
   * Returns the String containing the usage message for command-line options.
   *
   * @param include_unpublicized If true, treat all unpublicized options
   * and option groups as publicized
   * @param group_names The list of option groups to include in the usage
   * message.  If empty and option groups are being used, will return usage
   * for all option groups that are not unpublicized.  If empty and option
   * groups are not being used, will return usage for all options that are
   * not unpublicized.
   */
  public String usage(boolean include_unpublicized, String... group_names) {
    if (!use_groups) {
      if (group_names.length > 0) {
        throw new IllegalArgumentException(
          "This instance of Options does not have any option groups defined");
      }
      return format_options(options, max_opt_len(options, include_unpublicized),
                            include_unpublicized);
    }
     
    List<OptionGroupInfo> groups = new ArrayList<OptionGroupInfo>();
    if (group_names.length > 0) {
      for (String group_name : group_names) {
        if (!group_map.containsKey(group_name))
          throw new IllegalArgumentException("invalid option group: " + group_name);
        OptionGroupInfo gi = group_map.get(group_name);
        if (!include_unpublicized && !gi.any_publicized())
          throw new IllegalArgumentException("group does not contain any publicized options: " + group_name);
        else
          groups.add(group_map.get(group_name));
      }
    } else { // return usage for all groups that are not unpublicized
      for (OptionGroupInfo gi : group_map.values()) {
        if ((gi.unpublicized || !gi.any_publicized()) && !include_unpublicized)
          continue;
        groups.add(gi);
      }
    }

    List<Integer> lengths = new ArrayList<Integer>();
    for (OptionGroupInfo gi : groups)
      lengths.add(max_opt_len(gi.optionList, include_unpublicized));
    int max_len = Collections.max(lengths);

    StringBuilderDelimited buf = new StringBuilderDelimited(eol);
    for (OptionGroupInfo gi : groups) {
      buf.append(String.format("%n%s:", gi.name));
      buf.append(format_options(gi.optionList, max_len, include_unpublicized));
    }

    return buf.toString();
  }

  /**
   * Format a list of options for use in generating usage messages.
   */
  private String format_options(List<OptionInfo> opt_list, int max_len, boolean include_unpublicized) {
    StringBuilderDelimited buf = new StringBuilderDelimited(eol);
    for (OptionInfo oi : opt_list) {
      if (oi.unpublicized && ! include_unpublicized)
        continue;
      String default_str = "";
      if (oi.default_str != null)
        default_str = String.format(" [default %s]", oi.default_str);
      String use = String.format("  %-" + max_len + "s - %s%s",
                                 oi.synopsis(), oi.description, default_str);
      buf.append(use);
    }
    return buf.toString();
  }

  /**
   * Returns the length of the longest synopsis message in a list of options.
   * Useful for aligning options in usage strings.
   */
  private int max_opt_len(List<OptionInfo> opt_list, boolean include_unpublicized) {
    int max_len = 0;
    for (OptionInfo oi : opt_list) {
      if (oi.unpublicized && ! include_unpublicized)
        continue;
      int len = oi.synopsis().length();
      if (len > max_len)
        max_len = len;
    }
    return max_len;
  }

  /**
   * Package-private accessors/utility methods that are needed by the
   * OptionsDoclet class to generate HTML documentation.
   */
  boolean isUsingGroups() {
    return use_groups;
  }

  boolean isUsingSingleDash() {
    return use_single_dash;
  }

  List<OptionInfo> getOptions() {
    return options;
  }

  Collection<OptionGroupInfo> getOptionGroups() {
    return group_map.values();
  }

  /**
   * Set the specified option to the value specified in arg_value.  Throws
   * an ArgException if there are any errors.
   */
  private void set_arg (OptionInfo oi, String arg_name, /*@Nullable*/ String arg_value)
    throws ArgException {

    Field f = oi.field;
    Class<?> type = oi.base_type;

    // Keep track of all of the options specified
    if (options_str.length() > 0)
      options_str += " ";
    options_str += arg_name;
    if (arg_value != null) {
      if (! arg_value.contains (" ")) {
        options_str += "=" + arg_value;
      } else if (! arg_value.contains ("'")) {
        options_str += "='" + arg_value + "'";
      } else if (! arg_value.contains ("\"")) {
        options_str += "=\"" + arg_value + "\"";
      } else {
        throw new ArgException("Can't quote for internal debugging: " + arg_value);
      }
    }
    // Argument values are required for everything but booleans
    if (arg_value == null) {
      if ((type != Boolean.TYPE)
          || (type != Boolean.class)) {
        arg_value = "true";
      } else {
        throw new ArgException ("Value required for option " + arg_name);
      }
    }

    try {
      if (type.isPrimitive()) {
        if (type == Boolean.TYPE) {
          boolean val;
          String arg_value_lowercase = arg_value.toLowerCase();
          if (arg_value_lowercase.equals ("true") || (arg_value_lowercase.equals ("t")))
            val = true;
          else if (arg_value_lowercase.equals ("false") || arg_value_lowercase.equals ("f"))
            val = false;
          else
            throw new ArgException ("Value \"%s\" for argument %s is not a boolean",
                                    arg_value, arg_name);
          arg_value = (val) ? "true" : "false";
          // System.out.printf ("Setting %s to %s%n", arg_name, val);
          f.setBoolean (oi.obj, val);
        } else if (type == Integer.TYPE) {
          int val;
          try {
            val = Integer.decode (arg_value);
          } catch (Exception e) {
            throw new ArgException ("Value \"%s\" for argument %s is not an integer",
                                    arg_value, arg_name);
          }
          f.setInt (oi.obj, val);
        } else if (type == Long.TYPE) {
          long val;
          try {
            val = Long.decode (arg_value);
          } catch (Exception e) {
            throw new ArgException ("Value \"%s\" for argument %s is not a long integer",
                                    arg_value, arg_name);
          }
          f.setLong (oi.obj, val);
        } else if (type == Float.TYPE) {
          Float val;
          try {
            val = Float.valueOf (arg_value);
          } catch (Exception e) {
            throw new ArgException ("Value \"%s\" for argument %s is not a float",
                                    arg_value, arg_name);
          }
          f.setFloat (oi.obj, val);
        } else if (type == Double.TYPE) {
          Double val;
          try {
            val = Double.valueOf (arg_value);
          } catch (Exception e) {
            throw new ArgException ("Value \"%s\" for argument %s is not a double",
                                    arg_value, arg_name);
          }
          f.setDouble (oi.obj, val);
        } else { // unexpected type
          throw new Error ("Unexpected type " + type);
        }
      } else { // reference type

        // If the argument is a list, add repeated arguments or multiple
        // blank separated arguments to the list, otherwise just set the
        // argument value.
        if (oi.list != null) {
          if (split_lists) {
            String[] aarr = arg_value.split ("  *");
            for (String aval : aarr) {
              Object val = get_ref_arg (oi, arg_name, aval);
              oi.list.add (val); // uncheck cast
            }
          } else {
            Object val = get_ref_arg (oi, arg_name, arg_value);
            oi.list.add (val);
          }
        } else {
          Object val = get_ref_arg (oi, arg_name, arg_value);
          f.set (oi.obj, val);
        }
      }
    } catch (ArgException ae) {
      throw ae;
    } catch (Exception e) {
      throw new Error ("Unexpected error ", e);
    }
  }

  /**
   * Create an instance of the correct type by passing the argument value
   * string to the constructor.  The only expected error is some sort
   * of parse error from the constructor.
   */
  private /*@NonNull*/ Object get_ref_arg (OptionInfo oi, String arg_name, 
                                           String arg_value) throws ArgException {

    Object val = null;
    try {
      if (oi.constructor != null) {
        val = oi.constructor.newInstance (arg_value);
      } else if (oi.base_type.isEnum()) {
        @SuppressWarnings({"unchecked","rawtypes"})
        Object tmpVal = getEnumValue ((Class<? extends Enum>)oi.base_type,
                                       arg_value);
        val = tmpVal;
      } else {
        if (oi.factory == null) {
          throw new Error("No constructor or factory for argument " + arg_name);
        }
        val = oi.factory.invoke (null, arg_value);
      }
    } catch (Exception e) {
      throw new ArgException ("Invalid argument (%s) for argument %s",
                              arg_value, arg_name);
    }

    assert val != null : "@SuppressWarnings(nullness)";
    return val;
  }

  /**
   * Behaves like {@link java.lang.Enum#valueOf}, except that <code>name</code>
   * is case insensitive and hyphen insensitive (hyphens can be used in place of
   * underscores).  This allows for greater flexibility when specifying enum
   * types as command-line arguments.
   */
  private <T extends Enum<T>> T getEnumValue(Class<T> enumType, String name) {
    T[] constants = enumType.getEnumConstants();
    if (constants == null)
      throw new IllegalArgumentException(enumType.getName() + " is not an enum type");
    for (T constant : constants)
      if (((Enum<?>) constant).name().equalsIgnoreCase(name.replace('-', '_')))
        return constant;
    // same error that's thrown by Enum.valueOf()
    throw new IllegalArgumentException(
      "No enum constant " + enumType.getCanonicalName() + "." + name);
  }

  /**
   * Returns a short name for the specified type for use in messages.
   */
  private static String type_short_name (Class<?> type) {

    if (type.isPrimitive())
      return type.getName();
    else if (type == File.class)
      return "filename";
    else if (type == Pattern.class)
      return "regex";
    else if (type.isEnum())
      return ("enum");
    else
      return UtilMDE.unqualified_name (type.getName()).toLowerCase();
  }

  /**
   * Returns a string containing all of the options that were set and their
   * arguments.  This is essentially the contents of args[] with all
   * non-options removed.
   * @see #settings()
   */
  public String get_options_str() {
    return (options_str);
  }

  /**
   * Returns a string containing the current setting for each option, in a
   * format that can be parsed by Options.  This differs from
   * get_options_str() in that it contains each known option exactly once:
   * it never contains duplicates, and it contains every known option even
   * if the option was not specified on the command line.
   */
  public String settings () {
    return settings(false);
  }

  /**
   * Returns a string containing the current setting for each option, in a
   * format that can be parsed by Options.  This differs from
   * get_options_str() in that it contains each known option exactly once:
   * it never contains duplicates, and it contains every known option even
   * if the option was not specified on the command line.
   *
   * @param include_unpublicized  If true, treat all unpublicized options
   * and option groups as publicized
   */
  public String settings (boolean include_unpublicized) {
    StringBuilderDelimited out = new StringBuilderDelimited(eol);

    // Determine the length of the longest name
    int max_len = max_opt_len(options, include_unpublicized);

    // Create the settings string
    for (OptionInfo oi : options) {
      String use = String.format ("%-" + max_len + "s = ", oi.long_name);
      try {
        use += oi.field.get (oi.obj);
      } catch (Exception e) {
        throw new Error ("unexpected exception reading field " + oi.field, e);
      }
      out.append(use);
    }

    return out.toString();
  }

  /**
   * Returns a description of all of the known options.
   * Each option is described on its own line in the output.
   */
  public String toString() {
    StringBuilderDelimited out = new StringBuilderDelimited(eol);

    for (OptionInfo oi: options) {
      out.append(oi);
    }

    return out.toString();
  }

  /**
   * Exceptions encountered during argument processing.
   */
  public static class ArgException extends Exception {
    static final long serialVersionUID = 20051223L;
    public ArgException (String s) { super (s); }
    public ArgException (String format, /*@Nullable*/ Object... args) {
      super (String.format (format, args));
    }
  }


  private static class ParseResult {
    /*@Nullable*/ String short_name;
    /*@Nullable*/ String type_name;
    String description;
    ParseResult(/*@Nullable*/ String short_name, /*@Nullable*/ String type_name, String description) {
      this.short_name = short_name;
      this.type_name = type_name;
      this.description = description;
    }
  }


  /**
   * Parse an option value and return its three components (short_name,
   * type_name, and description).  The short_name and type_name are null
   * if they are not specified in the string.
   */
  private static ParseResult parse_option (String val) {

    // Get the short name, long name, and description
    String short_name;
    String type_name;
    /*@NonNull*/ String description;

    // Get the short name (if any)
    if (val.startsWith("-")) {
      assert val.substring(2,3).equals(" ");
      short_name = val.substring (1, 2);
      description = val.substring (3);
    } else {
      short_name = null;
      description = val;
    }

    // Get the type name (if any)
    if (description.startsWith ("<")) {
      type_name = description.substring (1).replaceFirst (">.*", "");
      description = description.replaceFirst ("<.*> ", "");
    } else {
      type_name = null;
    }

    // Return the result
    return new ParseResult(short_name, type_name, description);
  }

//   /**
//    * Test class with some defined arguments.
//    */
//   private static class Test {
//
//     @Option ("generic") List<Pattern> lp = new ArrayList<Pattern>();
//     @Option ("-a <filename> argument 1") String arg1 = "/tmp/foobar";
//     @Option ("argument 2") String arg2;
//     @Option ("-d double value") double temperature;
//     @Option ("-f the input file") File input_file;
//   }
//
//   /**
//    * Simple example
//    */
//   private static void main (String[] args) throws ArgException {
//
//     Options options = new Options ("test", new Test());
//     System.out.printf ("Options:%n%s", options);
//     options.parse_or_usage (args);
//     System.out.printf ("Results:%n%s", options.settings());
//   }

}
