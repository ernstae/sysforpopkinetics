public class ParserMain
{
    public static void main(String[] args)   
    { 
        NonmemParser parser = new NonmemParser();
        String messageControl = parser.parseControl(args[0]);
        if(!messageControl.startsWith("Error"));
        {
            String messageData = parser.parseData();
            if(!messageData.startsWith("Error"));
            {
                XMLGenerator xmlGenerator = new XMLGenerator(parser.getControl(), 
                                                             parser.getData()); 
	        xmlGenerator.setContent();
	        xmlGenerator.setDriver();
	        xmlGenerator.setModel();
                xmlGenerator.setData();
                xmlGenerator.setOutput();
                xmlGenerator.save(args[1]);
                System.out.println(xmlGenerator.getDocument());
            }
	}      
    }
}
