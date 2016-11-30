// Lists all declaration annotations (more specifically, non-type
// annotations) reachable from the classpath.  Only lists the simple name.

// Compile with:
//   javac -g DeclarationAnnotations.java -cp $pl/java/lib/guava-19.0.jar
// Run with:
//   java -cp ..:$pl/java/lib/guava-19.0.jar plume.DeclarationAnnotations
//   java -cp $pl/java/lib/guava-19.0.jar:$ch/dist/checker.jar:$ch/dist/javac.jar:${CLASSPATH}:.. plume.DeclarationAnnotations
// However, the latter invocation is problematic because Guava contains
// declaration versions of annotations such as @NonNull.  I don't want to
// have to determine the package of each annotation, so I won't use this.

package plume;

import com.google.common.reflect.ClassPath;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import java.util.Set;

// Do I need to be able to add other files, such as the Checker Framework?

public class DeclarationAnnotations {

  static boolean debug = false;

  public static void main(String[] args) throws IOException {

    System.out.println("declarationAnnotationsBuiltin = set([");

    ClassLoader cl = DeclarationAnnotations.class.getClassLoader();
    @SuppressWarnings("nullness") // unannotated Guava library
    Set<ClassPath.ClassInfo> classes = ClassPath.from(cl).getAllClasses();
    for (ClassPath.ClassInfo ci : classes) {
      if (debug) {
        System.out.println("Trying: " + ci);
      }
      Class<?> c;
      try {
        c = ci.load();
      } catch (Throwable e) {
        if (debug) {
          System.out.printf("Trouble in load(): %s: %s \"%s\"%n", ci, e.getClass(), e.getMessage());
        }
        continue;
      }
      if (!c.isAnnotation()) continue;
      Annotation anno = null;
      try {
        anno = c.getAnnotation(Target.class);
      } catch (Throwable e) {
        if (debug) {
          System.out.printf(
              "Trouble in getAnnotation(Target.class): %s: %s: %s \"%s\"%n",
              ci, c, e.getClass(), e.getMessage());
        }
      }
      if (anno != null) {
        Target t = (Target) anno;
        boolean isTypeAnnotation = false;
        for (ElementType et : t.value()) {
          // In Java 8, this can be "if (et == ElementType.TYPE_USE)".
          if (et.name().equals("TYPE_USE")) {
            isTypeAnnotation = true;
            break;
          }
        }
        // Debugging output
        // System.out.println((isTypeAnnotation ? "type        " : "declaration ") + ci.getName());
        if (!isTypeAnnotation) {
          System.out.println("  \"" + ci.getSimpleName() + "\", # " + ci.getName());
        }
      }
    }

    System.out.println("])");
  }
}
