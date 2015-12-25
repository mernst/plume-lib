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
 * Used in conjunction with the <code>@Option</code> annotation to indicate that
 * the following <code>@Option</code>-annotated fields (including the one the
 * <code>@OptionGroup</code> annotation is applied to) belong to the same
 * option group.  Option groups are documented in {@link Options}.
 * <p>
 * 
 * Note that <code>@OptionGroup</code> must be applied to a field and must
 * appear after that field's Javadoc comment, if any.  A Javadoc comment
 * between {@code @OptionGroup} and the field is ignored.
 *
 * For example, you must write
 * <pre>
 * /** comment for first option{@literal *}/
 *{@literal @}OptionGroup("the group")
 *{@literal @}Option("the first option")
 * public static int first_option = blah;</pre>
 * instead of
 * <pre>
 *
 *{@literal @}OptionGroup("the group")
 *
 * /** comment for first option{@literal *}/
 *{@literal @}Option("the first option")
 * public static int first_option = blah;</pre>
 *
 * @see plume.Options
 * @see plume.Option
 * @see plume.Unpublicized
 * @see plume.OptionsDoclet
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface OptionGroup {
  String value();
  boolean unpublicized() default false;
}
