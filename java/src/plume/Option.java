// The five files
//   Option.java
//   OptionGroup.java
//   Options.java
//   Unpublicized.java
//   OptionsDoclet.java
// together comprise the implementation of command-line processing.

package plume;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/*>>>
import org.checkerframework.framework.qual.IgnoreInWholeProgramInference;
*/

/**
 * Indicates that the annotated field is set via command-line option. Takes a single string argument
 * that describes the option. The string is in the format '{@code [-c] [<type>] description}':
 *
 * <ul>
 *   <li>'-c' is an optional single-character short name for the option.
 *   <li>'{@code <type>}' is an optional description of the option type, to be displayed instead of
 *       its Java type (e.g., '{@code <filename>}' if the variable's type is String). The less-than
 *       and greater-than symbols are required.
 *   <li>'description' is a short (one-line) description of the option suitable for a usage message.
 *       By convention, it starts with a capital letter but does not end with a period. The field's
 *       Javadoc comment should contain a longer description appropriate for a manual or manpage.
 *       The Javadoc comment often repeats something similar to the {@code @Option} description.
 * </ul>
 *
 * <p>The command-line options are processed by the {@link plume.Options} class. For example usage,
 * see the documentation for {@link plume.Options}.
 *
 * @deprecated Use org.plumelib.options.Option
 * @see plume.Options
 * @see plume.OptionGroup
 * @see plume.Unpublicized
 * @see plume.OptionsDoclet
 */
@Deprecated
/*@IgnoreInWholeProgramInference*/
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Option {
  /**
   * String that describes the option.
   *
   * @return a string that describes this option
   * @see Option
   */
  String value();

  /**
   * Aliases for this option. For information about the use of this field, see the section on Option
   * aliases in {@link plume.Options}. If there is only a single, one-character alias, it can be put
   * at the beginning of the value field without the need for an aliases field.
   *
   * @return aliases for this option
   */
  String[] aliases() default {};

  /**
   * Whether not to hide default values. For information about this parameter, see the section on
   * hiding default values in {@link plume.OptionsDoclet}.
   *
   * @return whether not to hide default values
   */
  boolean noDocDefault() default false;
}
