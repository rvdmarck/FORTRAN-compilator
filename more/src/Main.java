import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        try {
            LexicalAnalyzer la = new LexicalAnalyzer(new FileReader(args[0]));
            Parser p = new Parser(la);
            p.run();
        } catch (IOException e) {
            System.out.println("File Not Found: " + args[0]);
        } catch (ParserException e) {
            System.err.println(e);
        }
    }
}
