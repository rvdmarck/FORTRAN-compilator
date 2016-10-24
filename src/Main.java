import java.io.*;

public class Main {
public static void main(String[] args){
	Symbol sym;
	LexicalAnalyzer lexer = new LexicalAnalyzer(new FileReader(args[0]));
	for (sym = lexer.next_token(); sym.sym != 0; sym = lexer.next_token())
		System.out.println(sym);
}
}
