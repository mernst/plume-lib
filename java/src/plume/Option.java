// The four files
//   Option.java
//   OptionGroup.java
//   Options.java
//   Unpublicized.java
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
 *       specific than its Java type (eg, '&lt;filename&gt;' if the variable's
 *       type is String).  The less-than and greater-than symbols are required.
 *   <li>'description' is a short (typically one-line) description of the
 *       option suitable for a usage message.  The field's Javadoc comment
 *       can contain a longer description.
 * </ul>
 * <p>
 *
 * The command-line options are processed by the {@link plume.Options} class.
 * For example usage, see the documentation for {@link plume.Options}.
 * @see plume.Options
 * @see plume.Unpublicized
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Option {
  String value();
  String[] aliases() default "";
}
