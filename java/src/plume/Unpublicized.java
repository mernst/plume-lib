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

/**
 * Used in conjunction with the {@code @Option} annotation to signal that the option should not be
 * included in the usage message, unless an optional include_unpublicized argument is supplied to
 * the usage message method and is true.
 *
 * @deprecated Use <a
 *     href="http://plumelib.org/options/api/org/plumelib/options/Unpublicized.html">org.plumelib.options.Unpublicized</a>
 * @see plume.Option
 * @see plume.Options
 * @see plume.OptionGroup
 * @see plume.OptionsDoclet
 */
@Deprecated
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Unpublicized {}
