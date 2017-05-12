// This code is lifted from examples/Manifest.java.

package plume;

import java.io.FileInputStream;
import java.io.IOException;
import java.security.DigestInputStream;
import java.security.MessageDigest;

/*>>>
import org.checkerframework.common.value.qual.*;
*/

/**
 * Computes a message digest for a file. The primary entry point into this class is {@link
 * #getFileDigest(String,MessageDigest)}.
 */
public final class Digest {

  /** This class is a collecton of methods; it does not represent anything. */
  private Digest() {
    throw new Error("do not instantiate");
  }

  /**
   * This convenience method is used by both create() and verify(). It reads the contents of a named
   * file and computes a message digest for it, using the specified MessageDigest object.
   *
   * @param filename the file to read
   * @param md the MessageDigest
   * @return the message digest
   * @throws IOException if there is a problem reading the file
   */
  public static byte[] getFileDigest(String filename, MessageDigest md) throws IOException {
    // Make sure there is nothing left behind in the MessageDigest
    md.reset();

    // Create a stream to read from the file and compute the digest
    DigestInputStream in = new DigestInputStream(new FileInputStream(filename), md);

    // Read to the end of the file, discarding everything we read. {
    // The DigestInputStream automatically passes all the bytes read to
    // the update() method of the MessageDigest
    while (in.read(buffer) != -1) {
      /* do nothing */
    }

    // Finally, compute and return the digest value.
    byte[] result = md.digest();
    in.close();
    return result;
  }

  /** This static buffer is used by {@link #getFileDigest}. */
  private static byte[] buffer = new byte[4096];

  /** This array is used to convert from bytes to hexadecimal numbers. */
  private static final char[] digits = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  };

  /**
   * A convenience method to convert an array of bytes to a String. We do this simply by converting
   * each byte to two hexadecimal digits. Something like Base 64 encoding is more compact, but
   * harder to encode.
   *
   * @param bytes the bytes to convert to a String
   * @return a String representation of the input bytes
   */
  @SuppressWarnings("upperbound") // array length: computed index is within known array length
  public static String hexEncode(byte[] bytes) {
    StringBuffer s = new StringBuffer(bytes.length * 2);
    for (int i = 0; i < bytes.length; i++) {
      byte b = bytes[i];
      s.append(digits[(b & 0xf0) >> 4]);
      s.append(digits[b & 0x0f]);
    }
    return s.toString();
  }

  /**
   * A convenience method to convert from a string of hexadecimal digits to an array of bytes. This
   * method is the reverse of {@link #hexEncode(byte[])}.
   *
   * @param s the String to convert to an array of bytes
   * @return the bytes equivalent to the input String
   */
  public static byte[] hexDecode(String s) throws IllegalArgumentException {
    try {
      int len = s.length();
      byte[] r = new byte[len / 2];
      for (int i = 0; i < r.length; i++) {
        @SuppressWarnings("index") // correlated length: two arrays with correlated, nonequal length
        int digit1 = s.charAt(i * 2), digit2 = s.charAt(i * 2 + 1);
        if ((digit1 >= '0') && (digit1 <= '9')) {
          digit1 -= '0';
        } else if ((digit1 >= 'a') && (digit1 <= 'f')) {
          digit1 -= 'a' - 10;
        }
        if ((digit2 >= '0') && (digit2 <= '9')) {
          digit2 -= '0';
        } else if ((digit2 >= 'a') && (digit2 <= 'f')) {
          digit2 -= 'a' - 10;
        }
        @SuppressWarnings("cast.unsafe") // Value Checker issue 1264
        /*@IntRange(from = -128, to = 127)*/ byte r_i = (byte) ((digit1 << 4) + digit2);
        r[i] = r_i;
      }
      return r;
    } catch (Exception e) {
      throw new IllegalArgumentException("hexDecode(): invalid input");
    }
  }
}
