package plume;

import java.io.File;
import java.io.PrintStream;
import java.util.Formatter;
import java.util.Iterator;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.classfile.Code;
import org.apache.bcel.classfile.Constant;
import org.apache.bcel.classfile.ConstantClass;
import org.apache.bcel.classfile.ConstantPool;
import org.apache.bcel.classfile.ConstantUtf8;
import org.apache.bcel.classfile.Field;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.ArrayType;
import org.apache.bcel.generic.ClassGen;
import org.apache.bcel.generic.CodeExceptionGen;
import org.apache.bcel.generic.ConstantPoolGen;
import org.apache.bcel.generic.InstructionHandle;
import org.apache.bcel.generic.InstructionList;
import org.apache.bcel.generic.InstructionTargeter;
import org.apache.bcel.generic.LineNumberGen;
import org.apache.bcel.generic.LocalVariableGen;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.ObjectType;
import org.apache.bcel.generic.RETURN;
import org.apache.bcel.generic.Type;

/*>>>
import org.checkerframework.checker.index.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.signature.qual.*;
import org.checkerframework.common.value.qual.*;
*/

/**
 * Static utility methods for working with BCEL.
 *
 * @deprecated use org.plumelib.bcelutil instead
 */
@Deprecated
public final class BCELUtil {
  /** This class is a collection of methods; it does not represent anything. */
  private BCELUtil() {
    throw new Error("do not instantiate");
  }

  /** Controls whether the checks in checkMgen are actually performed. */
  public static boolean skip_checks = false;

  /** The type that represents String[]. */
  private static final Type string_array = Type.getType("[Ljava.lang.String;");

  /**
   * Prints method declarations to System.out.
   *
   * @param gen class whose methods to print
   */
  static void dump_method_declarations(ClassGen gen) {
    System.out.printf("method signatures for class %s%n", gen.getClassName());
    for (Method m : gen.getMethods()) {
      System.out.printf("  %s%n", get_method_declaration(m));
    }
  }

  /**
   * Returns a string describing a method declaration. It contains the access flags (public,
   * private, static, etc), the return type, the method name, and the types of each of its
   * arguments.
   *
   * @param m the method
   * @return a string describing the method declaration
   */
  public static String get_method_declaration(Method m) {

    StringBuilder sb = new StringBuilder();
    Formatter f = new Formatter(sb);

    f.format("%s %s %s (", get_access_flags(m), m.getReturnType(), m.getName());
    for (Type at : m.getArgumentTypes()) {
      f.format("%s, ", at);
    }
    f.format(")");
    return (sb.toString().replace(", )", ")"));
  }

  /**
   * Return a string representation of the access flags of method m.
   *
   * @param m the method whose access flags to retrieve
   * @return a string representation of the access flags of method m
   */
  static String get_access_flags(Method m) {

    int flags = m.getAccessFlags();

    StringBuilder buf = new StringBuilder();
    for (int i = 0, pow = 1; i <= Const.MAX_ACC_FLAG; i++) {
      if ((flags & pow) != 0) {
        if (buf.length() > 0) {
          buf.append(" ");
        }
        if (i < Const.ACCESS_NAMES_LENGTH) {
          buf.append(Const.getAccessName(i));
        } else {
          buf.append(String.format("ACC_BIT %x", pow));
        }
      }
      pow <<= 1;
    }

    return (buf.toString());
  }

  /**
   * Return the attribute name for the specified attribute.
   *
   * @param a the attribute
   * @return the attribute name for the specified attribute
   */
  public static String get_attribute_name(Attribute a) {

    ConstantPool pool = a.getConstantPool();
    int con_index = a.getNameIndex();
    Constant c = pool.getConstant(con_index);
    String att_name = ((ConstantUtf8) c).getBytes();
    return (att_name);
  }

  /**
   * Returns the constant string at the specified offset.
   *
   * @param pool the constant pool
   * @param index the index in the constant pool
   * @return the constant string at the specified offset in the constant pool
   */
  public static String get_constant_str(ConstantPool pool, int index) {

    Constant c = pool.getConstant(index);
    assert c != null : "Bad index " + index + " into pool";
    if (c instanceof ConstantUtf8) {
      return ((ConstantUtf8) c).getBytes();
    } else if (c instanceof ConstantClass) {
      ConstantClass cc = (ConstantClass) c;
      return cc.getBytes(pool) + " [" + cc.getNameIndex() + "]";
    } else {
      throw new Error("unexpected constant " + c + " class " + c.getClass());
    }
  }

  /**
   * Returns whether or not the method is a constructor.
   *
   * @param mg the method to test
   * @return true iff the method is a constructor
   */
  public static boolean is_constructor(MethodGen mg) {
    return (mg.getName().equals("<init>") || mg.getName().equals(""));
  }

  /**
   * Returns whether or not the method is a constructor.
   *
   * @param m the method to test
   * @return true iff the method is a constructor
   */
  public static boolean is_constructor(Method m) {
    return (m.getName().equals("<init>") || m.getName().equals(""));
  }

  /**
   * Returns whether or not the method is a class initializer.
   *
   * @param mg the method to test
   * @return true iff the method is a class initializer
   */
  public static boolean is_clinit(MethodGen mg) {
    return (mg.getName().equals("<clinit>"));
  }

  /**
   * Returns whether or not the method is a class initializer.
   *
   * @param m the method to test
   * @return true iff the method is a class initializer
   */
  public static boolean is_clinit(Method m) {
    return (m.getName().equals("<clinit>"));
  }

  /**
   * Returns whether or not the class is part of the JDK (rt.jar).
   *
   * @param gen the class to test
   * @return true iff the class is part of the JDK (rt.jar)
   */
  public static boolean in_jdk(ClassGen gen) {
    return (in_jdk(gen.getClassName()));
  }

  /**
   * Returns whether or not the class is part of the JDK (rt.jar).
   *
   * @param classname the class to test, in the format of Class.getName(); the class should not be
   *     an array
   * @return true iff the class is part of the JDK (rt.jar)
   */
  public static boolean in_jdk(/*@ClassGetName*/ String classname) {
    return classname.startsWith("java.")
        || classname.startsWith("com.oracle.")
        || classname.startsWith("com.sun.")
        || classname.startsWith("javax.")
        || classname.startsWith("jdk.")
        || classname.startsWith("org.ietf.")
        || classname.startsWith("org.jcp.")
        || classname.startsWith("org.omg.")
        || classname.startsWith("org.w3c.")
        || classname.startsWith("org.xml.")
        || classname.startsWith("sun.")
        || classname.startsWith("sunw.");
  }

  /**
   * Returns whether or not the class is part of the JDK (rt.jar).
   *
   * @param classname the class to test, in internal form
   * @return true iff the class is part of the JDK (rt.jar)
   */
  public static boolean in_jdk_internalform(/*@InternalForm*/ String classname) {
    return classname.startsWith("java/")
        || classname.startsWith("com/oracle/")
        || classname.startsWith("com/sun/")
        || classname.startsWith("javax/")
        || classname.startsWith("jdk/")
        || classname.startsWith("org/ietj/")
        || classname.startsWith("org/jcp/")
        || classname.startsWith("org/omg/")
        || classname.startsWith("org/w3c/")
        || classname.startsWith("org/xml/")
        || classname.startsWith("sun/")
        || classname.startsWith("sunw/");
  }

  /**
   * Print the methods in the class, to standard output.
   *
   * @param gen the class whose methods to print
   */
  static void dump_methods(ClassGen gen) {

    System.out.printf("Class %s methods:%n", gen.getClassName());
    for (Method m : gen.getMethods()) {
      System.out.printf("  %s%n", m);
    }
  }

  /**
   * Checks the specific method for consistency.
   *
   * @param mgen the class to check
   */
  public static void checkMgen(MethodGen mgen) {

    if (skip_checks) {
      return;
    }

    try {
      mgen.toString(); // ensure it can be formatted without exceptions
      mgen.getLineNumberTable(mgen.getConstantPool());

      InstructionList ilist = mgen.getInstructionList();
      if (ilist == null || ilist.getStart() == null) {
        return;
      }
      CodeExceptionGen[] exceptionHandlers = mgen.getExceptionHandlers();
      for (CodeExceptionGen gen : exceptionHandlers) {
        assert ilist.contains(gen.getStartPC())
            : "exception handler "
                + gen
                + " has been forgotten in "
                + mgen.getClassName()
                + "."
                + mgen.getName();
      }
      MethodGen nmg = new MethodGen(mgen.getMethod(), mgen.getClassName(), mgen.getConstantPool());
      nmg.getLineNumberTable(mgen.getConstantPool());
    } catch (Throwable t) {
      System.out.printf("failure in method %s.%s%n", mgen.getClassName(), mgen.getName());
      t.printStackTrace();
      throw new Error(t);
    }
  }

  /**
   * Checks all of the methods in gen for consistency.
   *
   * @param gen the class to check
   */
  public static void checkMgens(final ClassGen gen) {

    if (skip_checks) {
      return;
    }

    Method[] methods = gen.getMethods();
    for (int i = 0; i < methods.length; i++) {
      Method method = methods[i];
      // System.out.println ("Checking method " + method + " in class "
      // + gen.getClassName());
      checkMgen(new MethodGen(method, gen.getClassName(), gen.getConstantPool()));
    }

    if (false) {
      Throwable t = new Throwable();
      t.fillInStackTrace();
      StackTraceElement[] ste = t.getStackTrace();
      if (ste.length < 2) {
        System.out.println("No stack trace information available");
      } else {
        StackTraceElement caller = ste[1];
        System.out.printf(
            "%s.%s (%s line %d)",
            caller.getClassName(),
            caller.getMethodName(),
            caller.getFileName(),
            caller.getLineNumber());
        for (int ii = 2; ii < ste.length; ii++) {
          System.out.printf(" [%s line %d]", ste[ii].getFileName(), ste[ii].getLineNumber());
        }
        System.out.printf("%n");
      }
      dump_methods(gen);
    }
  }

  /**
   * Adds code in nl to start of method mg.
   *
   * @param mg method to be augmented
   * @param nl instructions to prepend to the method
   */
  public static void add_to_start(MethodGen mg, InstructionList nl) {

    // Add the code before the first instruction
    InstructionList il = mg.getInstructionList();
    InstructionHandle old_start = il.getStart();
    InstructionHandle new_start = il.insert(nl);

    // Move any LineNumbers and local variable that currently point to
    // the first instruction to include the new instructions. Other
    // targeters (branches, exceptions) should not include the new
    // code
    if (old_start.hasTargeters()) {
      // getTargeters() returns non-null because hasTargeters => true
      for (InstructionTargeter it : old_start.getTargeters()) {
        if ((it instanceof LineNumberGen) || (it instanceof LocalVariableGen)) {
          it.updateTarget(old_start, new_start);
        }
      }
    }
    mg.setMaxStack();
    mg.setMaxLocals();
  }

  /**
   * Dumps the contents of the specified class to the specified directory. The file is named
   * dump_dir/[class].bcel. It contains a synopsis of the fields and methods followed by the jvm
   * code for each method.
   *
   * @param jc javaclass to dump
   * @param dump_dir directory in which to write the file
   * @see #dump(JavaClass, File)
   */
  public static void dump(JavaClass jc, String dump_dir) {

    dump(jc, new File(dump_dir));
  }

  /**
   * Dumps the contents of the specified class to the specified directory. The file is named
   * dump_dir/[class].bcel. It contains a synopsis of the fields and methods followed by the jvm
   * code for each method.
   *
   * @param jc javaclass to dump
   * @param dump_dir directory in which to write the file
   */
  public static void dump(JavaClass jc, File dump_dir) {

    try {
      dump_dir.mkdir();
      File path = new File(dump_dir, jc.getClassName() + ".bcel");
      PrintStream p = new PrintStream(path);

      // Print the class, super class and interfaces
      p.printf("class %s extends %s%n", jc.getClassName(), jc.getSuperclassName());
      String[] inames = jc.getInterfaceNames();
      if ((inames != null) && (inames.length > 0)) {
        p.printf("   ");
        for (String iname : inames) {
          p.printf("implements %s ", iname);
        }
        p.printf("%n");
      }

      // Print each field
      p.printf("%nFields%n");
      for (Field f : jc.getFields()) {
        p.printf("  %s%n", f);
      }

      // Print the signature of each method
      p.printf("%nMethods%n");
      for (Method m : jc.getMethods()) {
        p.printf("  %s%n", m);
      }

      // If this is not an interface, print the code for each method
      if (!jc.isInterface()) {
        for (Method m : jc.getMethods()) {
          p.printf("%nMethod %s%n", m);
          Code code = m.getCode();
          if (code != null) {
            p.printf("  %s%n", code.toString().replace("\n", "\n  "));
          }
        }
      }

      // Print the details of the constant pool.
      p.printf("Constant Pool:%n");
      ConstantPool cp = jc.getConstantPool();
      Constant[] constants = cp.getConstantPool();
      for (int ii = 0; ii < constants.length; ii++) {
        p.printf("  %d %s%n", ii, constants[ii]);
      }

      p.close();

    } catch (Exception e) {
      throw new Error("Unexpected error dumping javaclass", e);
    }
  }

  // TODO: write Javadoc
  @SuppressWarnings("rawtypes")
  public static String instruction_descr(InstructionList il, ConstantPoolGen pool) {

    StringBuilder out = new StringBuilder();
    // not generic because BCEL is not generic
    for (Iterator i = il.iterator(); i.hasNext(); ) {
      @SuppressWarnings(
          "nullness") // BCEL's InstructionList is not generic but contains only non-null elements
      /*@NonNull*/ InstructionHandle handle = (InstructionHandle) i.next();
      out.append(handle.getInstruction().toString(pool.getConstantPool()) + "\n");
    }
    return (out.toString());
  }

  /**
   * Return a description of the local variables (one per line).
   *
   * @param mg the method whose local variables to describe
   * @return a description of the local variables (one per line)
   */
  public static String local_var_descr(MethodGen mg) {

    StringBuilder out = new StringBuilder();
    out.append(String.format("Locals for %s [cnt %d]%n", mg, mg.getMaxLocals()));
    LocalVariableGen[] lvgs = mg.getLocalVariables();
    if ((lvgs != null) && (lvgs.length > 0)) {
      for (LocalVariableGen lvg : lvgs) {
        out.append(String.format("  %s [index %d]%n", lvg, lvg.getIndex()));
      }
    }
    return (out.toString());
  }

  /**
   * Builds an array of line numbers for the specified instruction list. Each opcode is assigned the
   * next source line number starting at 1000.
   *
   * @param mg the method whose line numbers to extract
   * @param il the instruction list to augment with line numbers
   */
  public static void add_line_numbers(MethodGen mg, InstructionList il) {

    il.setPositions(true);
    for (InstructionHandle ih : il.getInstructionHandles()) {
      mg.addLineNumber(ih, 1000 + ih.getPosition());
    }
  }

  /**
   * Sets the locals to 'this' and each of the arguments. Any other locals are removed. An
   * instruction list with at least one instruction must exist.
   *
   * @param mg the method whose locals to set
   */
  public static void setup_init_locals(MethodGen mg) {

    // Get the parameter types and names.
    @SuppressWarnings(
        "nullness") // The arguments to the annotation aren't necessarily initialized before they
    // are written here. Since annotations are erased at runtime, this is safe.
    Type /*@SameLen({"arg_types", "mg.getArgumentTypes()"})*/[] arg_types = mg.getArgumentTypes();
    @SuppressWarnings(
        "nullness") // The arguments to the annotation aren't necessarily initialized before they
    // are written here. Since annotations are erased at runtime, this is safe.
    String /*@SameLen({"arg_types", "arg_names", "mg.getArgumentTypes()", "mg.getArgumentNames()"})*/
            []
        arg_names = mg.getArgumentNames();

    // Remove any existing locals
    mg.setMaxLocals(0);
    mg.removeLocalVariables();

    // Add a local for the instance variable (this)
    if (!mg.isStatic()) {
      mg.addLocalVariable("this", new ObjectType(mg.getClassName()), null, null);
    }

    // Add a local for each parameter
    for (int ii = 0; ii < arg_names.length; ii++) {
      mg.addLocalVariable(arg_names[ii], arg_types[ii], null, null);
    }

    // Reset the current number of locals so that when other locals
    // are added they get added at the correct offset
    mg.setMaxLocals();

    return;
  }

  /**
   * Empties the method of all code (except for a return). This includes line numbers, exceptions,
   * local variables, etc.
   *
   * @param mg the method to clear out
   */
  public static void empty_method(MethodGen mg) {

    mg.setInstructionList(new InstructionList(new RETURN()));
    mg.removeExceptionHandlers();
    mg.removeLineNumbers();
    mg.removeLocalVariables();
    mg.setMaxLocals();
  }

  /**
   * Remove the local variable type table attribute (LVTT) from mg. Evidently some changes require
   * this to be updated, but without BCEL support that would be hard to do. It should be safe to
   * just delete it since it is optional and really only of use to a debugger.
   *
   * @param mg the method to clear out
   */
  public static void remove_local_variable_type_tables(MethodGen mg) {

    for (Attribute a : mg.getCodeAttributes()) {
      if (is_local_variable_type_table(a, mg.getConstantPool())) {
        mg.removeCodeAttribute(a);
      }
    }
  }

  /**
   * Returns whether or not the specified attribute is a local variable type table.
   *
   * @param a the attribute
   * @param pool the constant pool
   * @return true iff the attribute is a local variable type table
   */
  public static boolean is_local_variable_type_table(Attribute a, ConstantPoolGen pool) {
    return (get_attribute_name(a, pool).equals("LocalVariableTypeTable"));
  }

  /**
   * Return the attribute name for the specified attribute.
   *
   * @param a the attribute
   * @param pool the constant pool
   * @return the attribute name for the specified attribute
   */
  public static String get_attribute_name(Attribute a, ConstantPoolGen pool) {

    int con_index = a.getNameIndex();
    Constant c = pool.getConstant(con_index);
    String att_name = ((ConstantUtf8) c).getBytes();
    return (att_name);
  }

  /**
   * Returns whether or not this is a standard main method (static, name is 'main', and one argument
   * of string array).
   *
   * @param mg the method to check
   * @return true iff the method is a main method
   */
  public static boolean is_main(MethodGen mg) {
    Type[] arg_types = mg.getArgumentTypes();
    return (mg.isStatic()
        && mg.getName().equals("main")
        && (arg_types.length == 1)
        && arg_types[0].equals(string_array));
  }

  /**
   * Returns the Java class name, in the format of {@link Class#getName()}, that corresponds to
   * type.
   *
   * @param type the type
   * @return the Java classname that corresponds to type
   */
  public static /*@ClassGetName*/ String type_to_classgetname(Type type) {
    String signature = type.getSignature();
    return UtilMDE.fieldDescriptorToClassGetName(signature);
  }

  /**
   * Returns the class that corresponds to type.
   *
   * @param type the type
   * @return the Java class that corresponds to type
   */
  public static Class<?> type_to_class(Type type) {

    String classname = type_to_classgetname(type);
    try {
      Class<?> c = UtilMDE.classForName(classname);
      return c;
    } catch (Exception e) {
      throw new RuntimeException("can't find class for " + classname, e);
    }
  }

  /**
   * Returns a copy of the given type array, with new_type added to the end.
   *
   * @param types the array to extend
   * @param new_type the element to add to the end of the array
   * @return a new array, with new_type at the end
   */
  public static Type[] postpendToArray(Type[] types, Type new_type) {
    Type[] new_types = new Type[types.length + 1];
    System.arraycopy(types, 0, new_types, 0, types.length);
    new_types[types.length] = new_type;
    Type[] new_types_cast = new_types;
    return (new_types_cast);
  }

  /**
   * Returns a copy of the given type array, with new_type added to the end.
   *
   * @deprecated use {@link #postpendToArray}
   * @param types the array to extend
   * @param new_type the element to add to the end of the array
   * @return a new array, with new_type at the end
   */
  @Deprecated
  public static Type[] add_type(Type[] types, Type new_type) {
    return postpendToArray(types, new_type);
  }

  /**
   * Returns a copy of the given type array, with new_type inserted at the beginning.
   *
   * @param types the array to extend
   * @param new_type the element to add to the beginning of the array
   * @return a new array, with new_type at the beginning
   */
  public static Type[] prependToArray(Type new_type, Type[] types) {
    @SuppressWarnings({
      "index", // new_types is @MinLen(1) except in the presence of overflow,
      // which the Value Checker accounts for, but the Index Checker does not.
      "value" // new_types is @MinLen(1) except in the presence of overflow,
      // which the Value Checker accounts for, but the Index Checker does not.
    })
    Type /*@MinLen(1)*/[] new_types = new Type[types.length + 1];
    System.arraycopy(types, 0, new_types, 1, types.length);
    new_types[0] = new_type;
    Type[] new_types_cast = new_types;
    return (new_types_cast);
  }

  /**
   * Returns a copy of the given type array, with new_type inserted at the beginning.
   *
   * @deprecated use {@link #prependToArray}
   * @param types the array to extend
   * @param new_type the element to add to the beginning of the array
   * @return a new array, with new_type at the beginning
   */
  @Deprecated
  public static Type[] insert_type(Type new_type, Type[] types) {
    return prependToArray(new_type, types);
  }

  /**
   * Return the type corresponding to a given fully-qualified class name.
   *
   * @param classname the fully-qualified name of a class
   * @return the type corresponding to the given class name
   */
  @SuppressWarnings("signature") // stripping off "[]" does not affect type
  public static Type classname_to_type(/*@ClassGetName*/ String classname) {

    // Get the array depth (if any)
    int array_depth = 0;
    while (classname.endsWith("[]")) {
      classname = classname.substring(0, classname.length() - 2);
      array_depth++;
    }
    classname = classname.intern();

    // Get the base type
    Type t = null;
    if (classname == "int") { // interned
      t = Type.INT;
    } else if (classname == "boolean") { // interned
      t = Type.BOOLEAN;
    } else if (classname == "byte") { // interned
      t = Type.BYTE;
    } else if (classname == "char") { // interned
      t = Type.CHAR;
    } else if (classname == "double") { // interned
      t = Type.DOUBLE;
    } else if (classname == "float") { // interned
      t = Type.FLOAT;
    } else if (classname == "long") { // interned
      t = Type.LONG;
    } else if (classname == "short") { // interned
      t = Type.SHORT;
    } else { // must be a non-primitive
      t = new ObjectType(classname);
    }

    // If there was an array, build the array type
    if (array_depth > 0) {
      t = new ArrayType(t, array_depth);
    }

    return t;
  }
}
