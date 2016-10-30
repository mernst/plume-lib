package plume;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * Given a list of .class files, print the class file version and also the JDK/JRE version required
 * to run it. A .jar file can also be supplied, in which case each .class file within it is
 * processed. Example use:
 *
 * <pre>  java ClassFileVersion MyClass.class</pre>
 *
 * Supplying the "-min JDKVER" argument suppresses output except for .class files that require at
 * least that JDK version. For instance, to list all the .class/.jar files that require JDK 6 or
 * later, in this or any subdirectory, run
 *
 * <pre>find . \( -name '*.class' -o -name '*.jar' \) -print | xargs java ClassFileVersion -min 6
 * </pre>
 */
public final class ClassFileVersion {

  /** This class is a collection of methods; it does not represent anything. */
  private ClassFileVersion() {
    throw new Error("do not instantiate");
  }

  /** Only report versions that are at least this large. */
  static double minversion = 0;

  /**
   * Main method for the ClassFileVersion program.
   *
   * @param args command-line arguments
   * @throws IOException if a file cannot be read
   */
  public static void main(String[] args) throws IOException {
    if (args.length == 0) {
      System.out.println("Supplied no arguments.");
      System.out.println("Usage: java ClassFileVersion [-min JDKVER] <.class or .jar files>");
      System.exit(1);
    }

    // Process and remove "-min JDKVER" command-line argument, if present.
    if ((args.length >= 2) && (args[0].equals("-min"))) {
      minversion = Double.parseDouble(args[1]);
      if (minversion == 1.6) {
        minversion = 6;
      } else if (minversion == 1.7) {
        minversion = 7;
      }
      args = ArraysMDE.subarray(args, 2, args.length - 2);
    }

    // System.out.println("newargs: " + java.util.Arrays.toString(args));

    for (String filename : args) {
      if (!new File(filename).exists()) {
        System.out.println(filename + " does not exist!");
        continue;
      }

      if (filename.endsWith(".class")) {
        try (FileInputStream fis = new FileInputStream(filename)) {
          processClassFile(filename, fis);
        }
      } else if (filename.endsWith(".jar")) {
        JarFile jarFile = new JarFile(filename);
        for (Enumeration<JarEntry> e = jarFile.entries(); e.hasMoreElements(); ) {
          JarEntry entry = e.nextElement();
          String entryName = entry.getName();
          // Should really process recursively included jar files...
          if (entryName.endsWith(".class")) {
            try (InputStream is = jarFile.getInputStream(entry)) {
              processClassFile(filename + ":" + entryName, is);
            }
          }
        }
      } else {
        System.out.println(filename + " is neither a .class nor a .jar file");
      }
    }
  }

  /**
   * Print, to standard out, the version number for the class file found in is.
   *
   * @param filename file name to appear in printed messages
   * @param is input stream from which to read classfile bytes to process
   */
  public static void processClassFile(String filename, InputStream is) {
    double[] versions = versionNumbers(is);
    if (versions == null) {
      System.out.println(filename + " is not a .class file (or IOException)");
    } else {
      double major = versions[0];
      double minor = versions[1];
      double jdkVersion = versions[2];

      if (jdkVersion >= minversion) {
        System.out.println(
            filename
                + " class file version is "
                + (int) major
                + "."
                + (int) minor
                + ", requires JDK "
                + ((jdkVersion == (int) jdkVersion)
                    ? Integer.toString((int) jdkVersion)
                    : Double.toString(jdkVersion))
                + " or later");
      }
    }
  }

  /**
   * Return an array of the major vernios, minor version, and JDK version of the class read from the
   * input stream. Return null if there is an error or the input isn't a class file.
   *
   * @param is input stream from which to read a class
   * @return array of three version numbers
   */
  public static double /*@Nullable*/ [] versionNumbers(InputStream is) {
    try {
      DataInputStream dis = new DataInputStream(is);
      int magic = dis.readInt();
      if (magic != 0xcafebabe) {
        return null;
      }

      double minor = dis.readShort();
      double major = dis.readShort();
      double jdkVersion;

      if (major < 48) {
        jdkVersion = 1.3; // really 1.3.1
      } else if (major == 48) {
        jdkVersion = 1.4; // really 1.4.2
      } else if (major == 49) {
        jdkVersion = 1.5;
      } else if (major == 50) {
        jdkVersion = 6;
      } else {
        jdkVersion = 7;
      }

      return new double[] {major, minor, jdkVersion};
    } catch (IOException e) {
      return null;
    }
  }
}
