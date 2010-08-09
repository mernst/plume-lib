// The five files
//   Option.java
//   OptionGroup.java
//   Options.java
//   Unpublicized.java
//   OptionsDoclet.java
// together comprise the implementation of command-line processing.

package plume;

import java.lang.annotation.*;

/**
 * Indicates that the annotated field is set via command-line option.
 * Takes a single string argument that describes the option.  The string
 * is in the format '[-c] [&lt;type&gt;] description':
 * <ul>
 *   <li>'-c' is an optional single-character short name for the option.
 *   <li>'&lt;type&gt;' is an optional description of the option type more
 *       specific than its Java type (e.g., '&lt;filename&gt;' if the variable's
 *       type is String).  The less-than and greater-than symbols are required.
 *   <li>'description' is a short (one-line) description of the option
 *       suitable for a usage message.  By convention. it does not end with
 *       a period.  The field's Javadoc comment should contain a longer
 *       description appropriate for a manual or manpage.  The Javadoc
 *       comment often repeats something similar to the @Option
 *       description.
 * </ul>
 * <p>
 *
 * For information about the <code>aliases</code> parameter, see the section on
 * Option aliases in {@link plume.Options}. <p>
 *
 * For information about the <code>docdefault</code> parameter, see the section
 * on default values in {@link plume.OptionsDoclet}. <p>
 *
 * The command-line options are processed by the {@link plume.Options} class.
 * For example usage, see the documentation for {@link plume.Options}.
 * @see plume.Options
 * @see plume.OptionGroup
 * @see plume.Unpublicized
 * @see plume.OptionsDoclet
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Option {
  String value();
  String[] aliases() default {};
  String docdefault() default "";
}
