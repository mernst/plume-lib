package plume;

import java.io.File;
import java.io.IOException;
import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.ParsingException;
import nu.xom.Serializer;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * Pretty-prints an HTML file, after converting it to valid XML. To use:
 *
 * <pre>java plume.HtmlPrettyPrint file.html &gt; filepp.html</pre>
 */
public final class HtmlPrettyPrint {

  /** This class is a collection of methods; it does not represent anything. */
  private HtmlPrettyPrint() {
    throw new Error("do not instantiate");
  }

  /**
   * Entry point for the HtmlPrettyPrint program.
   *
   * @param args command-line arguments
   */
  public static void main(String[] args) {

    for (String arg : args) {
      File f = new File(arg);
      String url = "file://" + f.getAbsolutePath();

      try {
        XMLReader tagsoup = XMLReaderFactory.createXMLReader("org.ccil.cowan.tagsoup.Parser");
        Builder parser = new Builder(tagsoup);

        // Parse the document
        Document document = parser.build(url);

        Serializer serializer = new Serializer(System.out);
        serializer.setIndent(2);
        serializer.setMaxLength(80);
        try {
          serializer.write(document);
        } catch (IOException ex) {
          System.err.println(ex);
        }
      } catch (ParsingException ex) {
        System.out.println(url + " is not well-formed.");
        throw new Error(ex);
      } catch (SAXException ex) {
        System.out.println("Could not load Xerces.");
        System.out.println(ex.getMessage());
      } catch (IOException ex) {
        System.out.println("IOException:  parser could not read " + url);
      }
    }
  }
}
