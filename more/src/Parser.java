import java.io.IOException;

/**
 * Created by acaccia on 02/12/16.
 */
public class Parser {
    private LexicalAnalyzer la;
    private Symbol peeked = null;

    public Parser(LexicalAnalyzer la){
        this.la = la;
    }

    private void peek() {
        try {
            peeked = la.yylex();
        } catch (IOException e) {
        }
    }

    private boolean match(Symbol s){
        if (s.equals(peeked)){
            peek();
            return true;
        }
        return false;
    }

    private void program(){

    }

    private void vars(){

    }

    private void varlist(){

    }

    private void followVarlist(){

    }

    private void code(){

    }

    private void instruction(){

    }

    private void assign(){

    }

    private void exprArithA(){

    }

    private void u(){

    }

    private void v(){

    }

    private void exprArithB(){

    }

    private void w(){

    }

    private void x(){

    }

    private void exprArithC(){

    }

    private void addOp(){

    }

    private void mulOp(){

    }

    private void ifrule(){

    }

    private void elserule(){

    }

    private void condA(){

    }

    private void a(){

    }

    private void b(){

    }

    private void condB(){

    }

    private void c(){

    }

    private void d(){

    }

    private void condC(){

    }

    private void simpleCond(){

    }

    private void comp(){

    }

    private void doRule(){

    }

    private void print(){

    }

    private void read(){

    }

    private void expList(){

    }

    private void followExplist(){

    }
}
