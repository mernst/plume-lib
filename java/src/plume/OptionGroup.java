// The four files
//   Option.java
//   OptionGroup.java
//   Options.java
//   Unpublicized.java
// together comprise the implementation of command-line processing.

package plume;

import java.lang.annotation.*;

/**
 * Used in conjunction with the <code>@Option</code> annotation to indicate that
 * the following <code>@Option</code>-annotated fields (including the one the
 * <code>@OptionGroup</code> annotation is being applied to) belong to the same
 * option group.
 * @see plume.Options
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface OptionGroup {
  String value();
  boolean unpublicized() default false;
}
