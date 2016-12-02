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

    private boolean match(LexicalUnit unit){
        LexicalUnit lu = peeked.getType();
        if (unit == lu){
            peek();
            return true;
        }
        return false;
    }

    private boolean matchAny(LexicalUnit... units){
        LexicalUnit lu = peeked.getType();
        for (LexicalUnit u : units){
            if (u == lu)
                return true;
        }
        return false;
    }

    private void program(){
        System.out.println("program");
        match(LexicalUnit.PROGRAM);
        match(LexicalUnit.VARNAME);
        match(LexicalUnit.ENDLINE);
        vars();
        code();
        match(LexicalUnit.END);
    }

    private void vars(){
        if (match(LexicalUnit.INTEGER)){
            System.out.println("vars1");
            varlist();
            match(LexicalUnit.ENDLINE);
        } else if(matchAny(LexicalUnit.ENDLINE, LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)){
            System.out.println("vars2");
        }else{
            System.out.println("vars3");
        }
    }

    private void varlist(){
        if(match(LexicalUnit.VARNAME)){
            System.out.println("varlist");
            followVarlist();
        }else if(matchAny(LexicalUnit.ENDLINE)){
            System.out.println("varlist2");
        }
    }

    private void followVarlist(){
        if (match(LexicalUnit.COMMA)){
            System.out.println("followvarlist1");
            varlist();
        } else {
            System.out.println("followvarlist2");
        }
    }

    private void code(){
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)){
            System.out.println("code1");
            instruction();
            match(LexicalUnit.ENDLINE);
            code();
        } else {
            System.out.println("code2");
        }
    }

    private void instruction(){
        switch (peeked.getType()){
            case VARNAME:
                System.out.println("instruction1");
                assign();
                break;
            case IF:
                System.out.println("instruction2");
                ifrule();
                break;
            case DO:
                System.out.println("instruction3");
                doRule();
                break;
            case PRINT:
                System.out.println("instruction4");
                print();
                break;
            case READ:
                System.out.println("instruction5");
                read();
                break;
            default:
                break;
        }
    }

    private void assign(){
        System.out.println("assign");
        match(LexicalUnit.VARNAME);
        match(LexicalUnit.EQUAL);
        exprArithA();
    }

    private void exprArithA(){
        System.out.println("exprArithA");
        u();
        v();
    }

    private void u(){
        System.out.println("u");
        exprArithB();
    }

    private void v(){
        if (matchAny(LexicalUnit.PLUS, LexicalUnit.MINUS)) {
            System.out.println("v1");
            addOp();
            exprArithB();
            v();
        } else {
            System.out.println("v2");
        }
    }

    private void exprArithB(){
        System.out.println("exprArithB");
        w();
        x();
    }

    private void w(){
        System.out.println("w");
        exprArithC();
    }

    private void x(){
        if (matchAny(LexicalUnit.TIMES, LexicalUnit.DIVIDE)){
            System.out.println("x1");
            mulOp();
            exprArithC();
            x()
        } else {
            System.out.println("x2");
        }
    }

    private void exprArithC(){
        if (match(LexicalUnit.VARNAME)){
            System.out.println("exprAritC1");
        } else if (match(LexicalUnit.NUMBER)){
            System.out.println("exprAritC2");
        } else if (match(LexicalUnit.LEFT_PARENTHESIS)){
            System.out.println("exprArithC3");
            exprArithA();
            match(LexicalUnit.RIGHT_PARENTHESIS);
        } else if (match(LexicalUnit.MINUS)){
            System.out.println("exprArithC4");
            exprArithA();
        }
    }

    private void addOp(){
        if (match(LexicalUnit.PLUS)){
            System.out.println("addop1");
        } else if (match(LexicalUnit.MINUS)){
            System.out.println("addop2");
        }
    }

    private void mulOp(){
        if (match(LexicalUnit.TIMES)){
            System.out.println("mulop1");
        } else if (match(LexicalUnit.DIVIDE)){
            System.out.println("mulop2");
        }
    }

    private void ifrule(){
        System.out.println("if");
        match(LexicalUnit.IF);
        condA();
        match(LexicalUnit.THEN);
        match(LexicalUnit.ENDLINE);
        code();
        elserule();
        match(LexicalUnit.ENDIF);
    }

    private void elserule(){
        if (match(LexicalUnit.ELSE)){
            System.out.println("elserule1");
            match(LexicalUnit.ENDLINE);
            code();
        } else {
            System.out.println("elserule2");
        }
    }

    private void condA(){
        System.out.println("condA");
        a();
        b();
    }

    private void a(){
        System.out.println("a");
        condB();
    }

    private void b(){
        if (match(LexicalUnit.OR)){
            System.out.println("b1");
            condB();
            b();
        } else {
            System.out.println("b2");
        }
    }

    private void condB(){
        System.out.println("condB");
        c();
        d();
    }

    private void c(){
        System.out.println("c");
        condC();
    }

    private void d(){
        if (match(LexicalUnit.AND)){
            System.out.println("d1");
            condC();
        } else {
            System.out.println("d2");
        }
    }

    private void condC(){
        System.out.println(match(LexicalUnit.NOT) ? "condC1" : "condC2");
        simpleCond();
    }

    private void simpleCond(){
        System.out.println("simpleCond");
        exprArithA();
        comp();
        exprArithA();
    }

    private void comp(){
        switch (peeked.getType()){
            case EQUAL_COMPARE:
                System.out.println("comp1");
                break;
            case GREATER_EQUAL:
                System.out.println("comp2");
                break;
            case GREATER:
                System.out.println("comp3");
                break;
            case SMALLER_EQUAL:
                System.out.println("comp4");
                break;
            case SMALLER:
                System.out.println("comp5");
                break;
            case DIFFERENT:
                System.out.println("comp6");
                break;
            default:
                break;
        }
    }

    private void doRule(){
        System.out.println("dorule");
        match(LexicalUnit.DO);
        match(LexicalUnit.VARNAME);
        match(LexicalUnit.EQUAL);
        match(LexicalUnit.NUMBER);
        match(LexicalUnit.COMMA);
        match(LexicalUnit.NUMBER);
        match(LexicalUnit.ENDLINE);
        code();
        match(LexicalUnit.ENDDO);
    }

    private void print(){
        System.out.println("print");
        match(LexicalUnit.PRINT);
        match(LexicalUnit.COMMA);
        expList();
    }

    private void read(){
        System.out.println("read");
        match(LexicalUnit.READ);
        match(LexicalUnit.COMMA);
        varlist();
    }

    private void expList(){
        System.out.println("explist");
        exprArithA();
        followExplist();
    }

    private void followExplist(){
        if (match(LexicalUnit.COMMA)){
            System.out.println("followExplist1");
            expList();
        } else {
            System.out.println("followExpList2");
        }
    }
}
