package plume;

/**
 * Routines for doing simple string formatting similar to printf/sprintf in C.
 * All of the arguments must be objects.  Primitive types can be used in
 * several ways:
 *
 * <ul>
 *   <li> Add them to a string:  "" + i
 *   <li> create a wrapper object:  new Integer(i)
 *   <li> use a fmt routine to create a wrapper object: Fmt.i(i)
 * </ul>
 *
 * @deprecated Use {@link String#format(String,Object...)}
 */
@Deprecated
public final class Fmt {

  /** This class is a collection of methods; it does not represent anything. */
  private Fmt() {
    throw new Error("do not instantiate");
  }

  /**
   * Writes, to System.out, te result of replacing each instance of %s in
   * format with the corresponding object in args.
   * Each argument is converted to a string with toString().
   * @param format the format string
   * @param args the values to be formatted
   */
  public static void pf(String format, Object[] args) {
    System.out.println(spf(format, args));
  }

  /**
   * Replaces each instance of %s in format with the corresponding
   * object in args and returns the result.  Each argument is
   * converted to a string with toString().
   * @param format the format string
   * @param args the values to be formatted
   * @return the formatted String
   */
  public static String spf(String format, Object[] args) {

    StringBuffer result = new StringBuffer(format.length() + args.length*20);

    int current_arg = 0;
    for (int i = 0; i < format.length(); i++) {
      char c = format.charAt(i);

      if (c != '%') {
        result.append(c);
      } else {
        i++;
        char cmd = format.charAt(i);
        if (cmd == '%') {
          result.append('%');
        } else if (cmd == 's') {
          if (args[current_arg] == null) {
            result.append("null");
          } else {
            Object arg = args[current_arg];
            if (arg instanceof long[]) {
              result.append(ArraysMDE.toString((long[])arg));
            } else if (arg instanceof String[]) {
              result.append(ArraysMDE.toString((String[])arg));
            } else if (arg instanceof double[]) {
              result.append(ArraysMDE.toString((double[])arg));
            } else {
              result.append(arg.toString());
            }
          }
          current_arg++;
        }
      }
    }

    if (current_arg != args.length) {
      throw new RuntimeException(
        spf("spf: only %s of %s arguments used up: [result = %s]",
            i(current_arg), i(args.length), result));
    }

    return (result.toString());
  }

  /**
   * Convenience routine for new Integer(val).
   * @param val int to convert to an Integer
   * @return the Integer equivalent of the input value
   **/
  public static Integer i(int val) {
    return new Integer(val);
  }

  public static String spf(String format, Object arg1) {
    return (spf(format, new Object[] {arg1}));
  }

  public static String spf(String format, Object arg1, Object arg2) {
    return (spf(format, new Object[] {arg1, arg2}));
  }

  public static String spf(String format, Object arg1, Object arg2,
                            Object arg3) {
    return (spf(format, new Object[] {arg1, arg2, arg3}));
  }

  public static String spf(String format, Object arg1, Object arg2,
                           Object arg3, Object arg4) {
    return (spf(format, new Object[] {arg1, arg2, arg3, arg4}));
  }

  public static String spf(String format, Object arg1, Object arg2,
                           Object arg3, Object arg4, Object arg5) {
    return (spf(format, new Object[] {arg1, arg2, arg3, arg4, arg5}));
  }

  public static void pf(String format) {
    pf(format, new Object[0]);
  }

  public static void pf(String format, Object arg1) {
    pf(format, new Object[] {arg1});
    return;
  }

  public static void pf(String format, Object arg1, Object arg2) {
    pf(format, new Object[] {arg1, arg2});
    return;
  }

  public static void pf(String format, Object arg1, Object arg2,
                            Object arg3) {
    pf(format, new Object[] {arg1, arg2, arg3});
    return;
  }

  public static void pf(String format, Object arg1, Object arg2,
                           Object arg3, Object arg4) {
    pf(format, new Object[] {arg1, arg2, arg3, arg4});
    return;
  }

  public static void pf(String format, Object arg1, Object arg2,
                           Object arg3, Object arg4, Object arg5) {
    pf(format, new Object[] {arg1, arg2, arg3, arg4, arg5});
    return;
  }

  static public void pf(String format, Object arg1, Object arg2,
                           Object arg3, Object arg4, Object arg5,
                           Object arg6) {
    pf(format, new Object[] {arg1, arg2, arg3, arg4, arg5, arg6});
    return;
  }

}
