import java.util.*;
import java.io.*;
import com.wutka.dtd.*;
import org.apache.oro.text.regex.*;

public class DTDDoc {
  public static void main(String[] args) throws IOException {
    DTDParser p = new DTDParser(new FileReader(args[0]));
    File dest = new File( args[1] );
    if( !dest.exists() )
    {
       System.out.println( dest.getPath() + " directory does not exist.  Creat it.\n" );
       dest.mkdir();
    } 
    DTD dtd = p.parse(true);
    Enumeration e = dtd.getItemsByType(DTDComment.class).elements();
    Hashtable comments = new Hashtable();
    while (e.hasMoreElements()) {
      Object o = e.nextElement();
      if (!(o instanceof DTDComment)) continue;
      parseComment(comments, ((DTDComment)o).getText());
    }
    Hashtable parents = findParents(dtd);
    writeOverviewPage(new FileWriter(dest.getPath()+File.separator+"dtd-overview.html"), dtd, comments);
    writeTreePage(new FileWriter(dest.getPath()+File.separator+"dtd-tree.html"), dtd, comments);
    writeIndexPage(new FileWriter(dest.getPath()+File.separator+"dtd-index.html"), dtd, comments);
    writeNotationsPage(new FileWriter(dest.getPath()+File.separator+"dtd-notations.html"), dtd, comments);
    writeEntitiesPage(new FileWriter(dest.getPath()+File.separator+"dtd-entities.html"), dtd, comments);
    e = dtd.elements.elements();
    while (e.hasMoreElements()) {
      DTDElement elt = (DTDElement) e.nextElement();
      writeElementPage(new FileWriter(dest.getPath()+File.separator+"elt-" + elt.name + ".html"), dtd, comments, parents, elt);
    }
  }

  private static void parseComment(Hashtable comments, String comment) {
    PatternCompiler pc = new Perl5Compiler();
    PatternMatcher pm = new Perl5Matcher();
    Pattern pattern;

    try {
      pattern = pc.compile("<(\\w*)(\\s+(\\w+)=\"[^\"]*\")?>\\s*(.*)(\n[\\w\\W]*)?", Perl5Compiler.MULTILINE_MASK);
    }
    catch (MalformedPatternException ex) {
      throw new IllegalArgumentException("invalid pattern");
    }
    PatternMatcherInput input = new PatternMatcherInput(comment);
    if (pm.contains(input, pattern)) {
      MatchResult result = pm.getMatch();
      String[] s = {htmlifyComment(result.group(4)), htmlifyComment(result.group(5))};
      comments.put(result.group(1) + " " + result.group(3),s);
    }
  }

  private static String getShortComment(Hashtable comments, DTDElement elt, DTDAttribute att) {
    String eltname = elt == null ? "" : elt.name;
    String attname = att == null ? "null" : att.name;
    String[] comment = (String[])comments.get(eltname + " " + attname);
    return comment[0];
  }

  private static String getLongComment(Hashtable comments, DTDElement elt, DTDAttribute att) {
    String eltname = elt == null ? "" : elt.name;
    String attname = att == null ? " null" : att.name;
    String[] comment = (String[])comments.get(eltname + attname);
    // Sachiko 10/22/03: Modified so that it won't attempt to
    // access the String array when it's empty.
    if( comment != null )
       return comment[1].equals("null") ? "" : comment[1];
    else
       return "";
  }

  private static Hashtable findParents(DTD dtd) throws IOException {
    Hashtable parents = new Hashtable();
    Enumeration e = dtd.elements.elements();
    while (e.hasMoreElements()) {
      DTDElement elt = (DTDElement) e.nextElement();
      StringWriter sw = new StringWriter();
      elt.content.write(new PrintWriter(sw));
      PatternCompiler pc = new Perl5Compiler();
      PatternMatcher pm = new Perl5Matcher();
      Pattern pattern;
      try {
	pattern = pc.compile("[ (](\\w+)");
      }
      catch (MalformedPatternException ex) {
	throw new IllegalArgumentException("invalid pattern");
      }
      PatternMatcherInput input = new PatternMatcherInput(sw.toString());
      while (pm.contains(input, pattern)) {
	MatchResult result = pm.getMatch();
	String child = result.group(1);
	Vector v = parents.containsKey(child) ? (Vector)parents.get(child) : new Vector();
	v.addElement(elt.name);
	parents.put(child, v);
      }
    }
    return parents;
  }

  private static void writeOverviewPage(Writer w, DTD dtd, Hashtable comments) throws IOException {
    writeHeader(w);
    w.write("<h1>Overview</h1>");
    w.write("<p>" + getShortComment(comments, null, null) + "</p>");
    w.write("<p>" + getLongComment(comments, null, null) + "</p>");
    String root = dtd.rootElement.name;
    w.write("<h2>Root element</h2><p><a href=elt-" + root + ".html>" + root + "</a></p>");
    writeFooter(w);
  }
  
  private static void writeTreePage(Writer w, DTD dtd, Hashtable comments) throws IOException {
    writeHeader(w);
    w.write("<h1>Element Tree</h1>");
    w.write("<ul>");
    writeTreeElement(w, dtd, comments, dtd.rootElement.name);
    w.write("</ul>");
    writeFooter(w);
  }
  
  private static void writeTreeElement(Writer w, DTD dtd, Hashtable comments, String eltName) throws IOException {
    DTDElement elt = (DTDElement)dtd.elements.get(eltName);
    w.write("<li><a href=elt-" + elt.name + ".html>" + elt.name + "</a>");
    w.write(" - " + getShortComment(comments, elt, null));
    DTDItem content = elt.getContent();
    if (content instanceof DTDContainer) {
      boolean first = true;
      Enumeration e = ((DTDContainer)content).getItemsVec().elements();
      while (e.hasMoreElements()) {
	DTDItem i = (DTDItem)e.nextElement();
	if (i instanceof DTDName) {
	  if (first) {
	    w.write("<ul>");
	    first = false;
	  }
	  writeTreeElement(w, dtd, comments,((DTDName)i).value);
	}
      }
      if (!first) w.write("</ul>");
    }
  }
  
  private static void writeIndexPage(Writer w, DTD dtd, Hashtable comments) throws IOException {
    writeHeader(w);
    w.write("<h1>Index</h1>");
    Vector v = new Vector();
    Enumeration e = dtd.elements.elements();
    while (e.hasMoreElements()) {
      DTDElement elt = (DTDElement)e.nextElement();
      insertIntoIndex(v, elt.name, "Element", getShortComment(comments, elt, null));
      Enumeration f = elt.attributes.elements();
      while (f.hasMoreElements()) {
	DTDAttribute att = (DTDAttribute) f.nextElement();
	insertIntoIndex(v, att.name + " (" + elt.name + ")", "Attribute", getShortComment(comments, elt, att));
      }
    }
    e = dtd.entities.elements();
    while (e.hasMoreElements()) {
      DTDEntity ent = (DTDEntity)e.nextElement();
      insertIntoIndex(v, ent.name, "Entity", ent.value);
    }
    e = dtd.notations.elements();
    while (e.hasMoreElements()) {
      DTDNotation not = (DTDNotation)e.nextElement();
      insertIntoIndex(v, not.name, "Notation", not.externalID.system);
    }
    w.write("<table border=1><tr><th>Name</th><th>Type</th><th>Value</th></tr>");
    e = v.elements();
    while (e.hasMoreElements()) {
      String line = (String)e.nextElement();
      w.write("<tr><td>" + line +"</td></tr>");
    }
    w.write("</table>");
    writeFooter(w);
  }
  
  private static void insertIntoIndex(Vector v, String name, String type, String value) {
    int i = 0;
    String sort = "<!--" + name + "-->";
    while (i < v.size() && sort.compareToIgnoreCase((String)v.elementAt(i)) > 0) i++;
    sort = sort + (type.equals("Element") ? "<a href=elt-" + name + ".html>" + name + "</a>" : name);
    v.insertElementAt(sort + "</td><td>" + type + "</td><td>" + value, i);
  }

  private static void writeNotationsPage(Writer w, DTD dtd, Hashtable comments) throws IOException {
    writeHeader(w);
    w.write("<h1>Notations</h1>");
    if (dtd.notations.size() == 0) {
      w.write("<p><em>None</em></p>");
    }
    else {
      Enumeration e = dtd.notations.elements();
      while (e.hasMoreElements()) {
	DTDNotation not = (DTDNotation)e.nextElement();
	w.write("<li>" + not.name + " = " + not.externalID.system + "</li>");
      }
    }
    writeFooter(w);
  }
  
  private static void writeEntitiesPage(Writer w, DTD dtd, Hashtable comments) throws IOException {
    writeHeader(w);
    w.write("<h1>Entities</h1>");
    if (dtd.entities.size() == 0) {
      w.write("<p><em>None</em></p>");
    }
    else {
      w.write("<ul>");
      Enumeration e = dtd.entities.elements();
      while (e.hasMoreElements()) {
	DTDEntity ent = (DTDEntity)e.nextElement();
	w.write("<li>" + ent.name + " = " + ent.value + "</li>");
      }
      w.write("</ul>");
    }
    writeFooter(w);
  }
  
  private static void writeElementPage(Writer w, DTD dtd, Hashtable comments, Hashtable parents, DTDElement elt) throws IOException {
    writeHeader(w);
    w.write("<h1>Element &lt;" + elt.name +"&gt;</h1>");
    w.write("<p>" + getShortComment(comments, elt, null) + "</p>");
    String comment = getLongComment(comments, elt, null);
    if (!comment.equals(""))
      w.write("<p>" + comment + "</p>");
    w.write("<h2>Attributes</h2>");
    if (elt.attributes.size() == 0) {
      w.write("<p><em>None</em></p>");
    }
    else {
      w.write("<dl>");
      Enumeration e = elt.attributes.elements();
      while (e.hasMoreElements()) {
	DTDAttribute att = (DTDAttribute)e.nextElement();
	String defaultValue = att.defaultValue == null ? "" : att.defaultValue;
	w.write("<dt>" + att.name + " " + att.decl.name + " " + defaultValue + "</dt>");
        w.write("<dd>" + "<p>" + getShortComment(comments, elt, att) + "</p>");
        // Sachiko added the following line in order to print out 
        // more than one line of comments on an attribute.
        comment = getLongComment(comments, elt, att);
        if( !comment.equals("") )
           w.write("<p>" + comment + "</p>");
        w.write( "</dd>" );
      }
      w.write("</dl>");
    }
    w.write("<h2>Content</h2>");
    StringWriter sw = new StringWriter();
    elt.content.write(new PrintWriter(sw));
    w.write("<p>" + htmlifyContent(sw.toString()) + "</p>");
    w.write("<h2>Parents</h2>");
    Vector p = (Vector)parents.get(elt.name);
    if (p == null) {
      w.write("<p><em>None</em></p>");
    }
    else {
      w.write("<p>");
      Enumeration e = p.elements();
      while (e.hasMoreElements()) {
	String parent = (String) e.nextElement();
	w.write("<a href=elt-" + parent + ".html>" + parent + "</a> ");
      }
      w.write("</p>");
    }
    writeFooter(w);
  }
  
  private static String htmlifyContent(String s) {
    String shtml;
    try {
      shtml = Util.substitute(new Perl5Matcher(), (new Perl5Compiler()).compile("([ (])(\\w+)"), new Perl5Substitution("$1<a href=elt-$2.html>$2</a>"), s, Util.SUBSTITUTE_ALL);
    }
    catch (MalformedPatternException ex) {
      throw new IllegalArgumentException("invalid pattern");
    }
    return shtml;
  }

  private static String htmlifyComment(String s) {
    PatternCompiler pc = new Perl5Compiler();
    PatternMatcher pm = new Perl5Matcher();
    String shtml;
    if (s == null) return "";
    try {
      shtml = Util.substitute(pm, pc.compile("<(\\w+)>"), new Perl5Substitution("<a href=elt-$1.html>$1</a>"), s, Util.SUBSTITUTE_ALL);
      shtml = Util.substitute(pm, pc.compile("html:"), new Perl5Substitution(), shtml, Util.SUBSTITUTE_ALL);
    }
    catch (MalformedPatternException ex) {
      throw new IllegalArgumentException("invalid pattern");
    }
    return shtml;
  }

  private static void writeHeader(Writer w) throws IOException {
    w.write("<html><body><p>");
    w.write("<a href=dtd-overview.html>Overview</a> ");
    w.write("<a href=dtd-tree.html>Tree</a> ");
    w.write("<a href=dtd-index.html>Index</a> ");
    w.write("<a href=dtd-notations.html>Notations</a> ");
    w.write("<a href=dtd-entities.html>Entities</a> ");
    w.write("</p>");
  }

  private static void writeFooter(Writer w) throws IOException {
    w.write("</body></html>");
    w.close();
  }
}
