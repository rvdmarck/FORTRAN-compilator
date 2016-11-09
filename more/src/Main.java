import java.io.*;

public class Main {
public static void main (String[] args){
	try {
		LexicalAnalyzer la = new LexicalAnalyzer(new FileReader(args[0]));
		Symbol sym = la.yylex();
		while (sym != null) {
			System.out.println(sym);
			sym = la.yylex();
		}
	} catch (IOException e) {
		System.out.println("File Not Found: " + args[0]);
	}
}
}
