import java.io.*;

public class Main {
public static void main (String[] args){
	try {
		LexicalAnalyzer la = new LexicalAnalyzer(new FileReader(args[0]));
		for (Symbol sym = la.yylex(); sym != null; sym = la.yylex())
			System.out.println(sym);
	} catch (IOException e) {
		System.out.println("File Not Found: " + args[0]);
	}
}
}
