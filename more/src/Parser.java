import java.io.IOException;

class Parser {
    private LexicalAnalyzer la;
    private Symbol peeked = null;

    /**
     * Uber-Fortran Parser
     * @param la LexicalAnalyzer containing code scan
     */
    Parser(LexicalAnalyzer la) {
        this.la = la;
    }

    /**
     * run the Parser
     * @throws ParserException
     */
    void run() throws ParserException {
        peek();
        program();
    }

    /**
     * peek the next token from lexical analyzer
     * May throw a runtime exception in case of IO Error
     */
    private void peek() {
        try {
            peeked = la.yylex();
        } catch (IOException e) {
            throw new RuntimeException("An IO error occurred");
        }
    }

    /**
     * check if unit match peeked
     * side effect: if it does, peek next token
     * @param unit to match with peeked
     * @return if it does match
     */
    private boolean match(LexicalUnit unit) {
        LexicalUnit lu = peeked.getType();
        if (unit == lu) {
            peek();
            return true;
        }
        return false;
    }

    /**
     * Check for matching unit in a list with the peeked one
     * @param units list of units to try
     * @return if at least one of the units match with peeked
     */
    private boolean matchAny(LexicalUnit... units) {
        LexicalUnit lu = peeked.getType();
        for (LexicalUnit u : units) {
            if (u == lu)
                return true;
        }
        return false;
    }

    /**
     * try to match or fail if it does not
     * @param lu lexical unit to check
     * @param rule rule number if it fails
     * @return true if it match
     * @throws ParserException
     */
    private boolean matchOrThrow(LexicalUnit lu, int rule) throws ParserException {
        if (!match(lu))
            throw new ParserException(peeked, rule);
        else
            return true;
    }

    /**
     * pretty printer for rules
     * @param n rule number
     * @param left part in the left of arrow
     * @param right part in the right of arrow
     */
    private void printRule(int n, String left, String right) {
        String nStr = "[" + n + "]";
        left = "<" + left + ">";
        System.out.println(String.format("%-5s %-15s \u2192 %s", nStr, left, right));
    }

    private void program() throws ParserException {
        printRule(1, "Program", "PROGRAM [ProgName] [EndLine] <Vars> <Code> END");
        matchOrThrow(LexicalUnit.PROGRAM, 1);
        matchOrThrow(LexicalUnit.VARNAME, 1);
        matchOrThrow(LexicalUnit.ENDLINE, 1);
        vars();
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT, LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)){
            code();
        } else {
            throw new ParserException(peeked, 1);
        }
        matchOrThrow(LexicalUnit.END, 1);
    }

    private void vars() throws ParserException {
        if (match(LexicalUnit.INTEGER)) {
            printRule(2, "Vars", "INTEGER <VarList> [EndLine]");
            varlist();
            matchOrThrow(LexicalUnit.ENDLINE, 2);
        } else if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)) {
            printRule(3, "Vars", "\u0395");
        } else {
            throw new ParserException(peeked, 1);
        }
    }

    private void varlist() throws ParserException {
        printRule(4, "VarList", "[VarName] <FollowVarList>");
        matchOrThrow(LexicalUnit.VARNAME, 4);
        followVarlist();
    }

    private void followVarlist() throws ParserException {
        if (match(LexicalUnit.COMMA)) {
            printRule(5, "FollowVarList", ", <VarList>");
            varlist();
        } else if (matchAny(LexicalUnit.ENDLINE)) {
            printRule(6, "FollowVarList", "\u0395");
        } else {
            throw new ParserException(peeked, 4);
        }
    }

    private void code() throws ParserException {
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)) {
            printRule(7, "Code", "<Instruction> [EndLine] <Code>");
            instruction();
            matchOrThrow(LexicalUnit.ENDLINE, 7);
            code();
        } else if (matchAny(LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)) {
            printRule(8, "Code", "\u0395");
        } else {
            throw new ParserException(peeked, 7);
        }
    }

    private void instruction() throws ParserException {
        switch (peeked.getType()) {
        case VARNAME:
            printRule(9, "Instruction", "<Assign>");
            assign();
            break;
        case IF:
            printRule(10, "Instruction", "<If>");
            ifrule();
            break;
        case DO:
            printRule(11, "Instruction", "<Do>");
            doRule();
            break;
        case PRINT:
            printRule(12, "Instruction", "<Print>");
            print();
            break;
        case READ:
            printRule(13, "Instruction", "<Read>");
            read();
            break;
        default:
            throw new ParserException(peeked, 7);
        }
    }

    private void assign() throws ParserException {
        printRule(14, "Assign", "[VarName] = <ExprArithA>");
        matchOrThrow(LexicalUnit.VARNAME, 14);
        matchOrThrow(LexicalUnit.EQUAL, 14);
        exprArithA();
    }

    private void exprArithA() throws ParserException {
        printRule(15, "ExprArithA", "<ExprArithB> <V>");
        exprArithB();
        v();
    }

    private void v() throws ParserException {
        if (matchAny(LexicalUnit.PLUS, LexicalUnit.MINUS)) {
            printRule(16, "V", "<AddOp> <ExprArithB> <V>");
            addOp();
            exprArithB();
            v();
        } else if (matchAny(LexicalUnit.COMMA, LexicalUnit.EQUAL, LexicalUnit.GREATER_EQUAL, LexicalUnit.GREATER, LexicalUnit.SMALLER_EQUAL,
                LexicalUnit.DIFFERENT, LexicalUnit.RIGHT_PARENTHESIS, LexicalUnit.TIMES, LexicalUnit.DIVIDE, LexicalUnit.ENDLINE,
                LexicalUnit.AND, LexicalUnit.OR)) {
            printRule(17, "V", "\u0395");
        } else {
            throw new ParserException(peeked, 15);
        }
    }

    private void exprArithB() throws ParserException {
        printRule(18, "ExprArithB", "<ExprArithC> <X>");
        exprArithC();
        x();
    }

    private void x() throws ParserException {
        if (matchAny(LexicalUnit.TIMES, LexicalUnit.DIVIDE)) {
            printRule(19, "X", "<MulOp> <ExprArithC> <X>");
            mulOp();
            exprArithC();
            x();
        } else if (matchAny(LexicalUnit.COMMA, LexicalUnit.EQUAL, LexicalUnit.GREATER_EQUAL, LexicalUnit.GREATER, LexicalUnit.SMALLER_EQUAL,
                LexicalUnit.DIFFERENT, LexicalUnit.RIGHT_PARENTHESIS, LexicalUnit.ENDLINE, LexicalUnit.AND, LexicalUnit.OR,
                LexicalUnit.PLUS, LexicalUnit.MINUS)) {
            printRule(20, "X", "\u0395");
        } else {
            throw new ParserException(peeked, 18);
        }
    }

    private void exprArithC() throws ParserException {
        if (match(LexicalUnit.VARNAME)) {
            printRule(21, "ExprArithC", "[VarName]");
        } else if (match(LexicalUnit.NUMBER)) {
            printRule(22, "ExprArithC", "[Number]");
        } else if (match(LexicalUnit.LEFT_PARENTHESIS)) {
            printRule(23, "ExprArithC", "( <ExprArithA> )");
            exprArithA();
            matchOrThrow(LexicalUnit.RIGHT_PARENTHESIS, 23);
        } else if (match(LexicalUnit.MINUS)) {
            printRule(24, "ExprArithC", "- <ExprArithA>");
            exprArithA();
        } else {
            throw new ParserException(peeked, 18);
        }
    }

    private void addOp() throws ParserException {
        if (match(LexicalUnit.PLUS)) {
            printRule(25, "AddOp", "+");
        } else if (match(LexicalUnit.MINUS)) {
            printRule(26, "AddOp", "-");
        } else {
            throw new ParserException(peeked, 16);
        }
    }

    private void mulOp() throws ParserException {
        if (match(LexicalUnit.TIMES)) {
            printRule(27, "MulOp", "*");
        } else if (match(LexicalUnit.DIVIDE)) {
            printRule(28, "MulOp", "/");
        } else {
            throw new ParserException(peeked, 19);
        }
    }

    private void ifrule() throws ParserException {
        printRule(29, "If", "IF ( <CondA> ) THEN [EndLine] <Code> <IfSeq>");
        matchOrThrow(LexicalUnit.IF, 29);
        matchOrThrow(LexicalUnit.LEFT_PARENTHESIS, 29);
        condA();
        matchOrThrow(LexicalUnit.RIGHT_PARENTHESIS, 29);
        matchOrThrow(LexicalUnit.THEN, 29);
        matchOrThrow(LexicalUnit.ENDLINE, 29);
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT, LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)){
            code();
        } else {
            throw new ParserException(peeked, 29);
        }
        ifSeq();
    }

    private void ifSeq() throws ParserException {
        if (match(LexicalUnit.ENDIF)) {
            printRule(30, "Else", "ENDIF");
        } else if (match(LexicalUnit.ELSE)) {
            printRule(31, "Else", "ELSE [EndLine] <Code> ENDIF");
            matchOrThrow(LexicalUnit.ENDLINE, 31);
            if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT, LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                    LexicalUnit.ENDIF)){
                code();
            } else {
                throw new ParserException(peeked, 31);
            }
            matchOrThrow(LexicalUnit.ENDIF, 31);
        } else {
            throw new ParserException(peeked, 29);
        }
    }

    private void condA() throws ParserException {
        printRule(32, "CondA", "<CondB> <B>");
        condB();
        b();
    }

    private void b() throws ParserException {
        if (match(LexicalUnit.OR)) {
            printRule(33, "B", ".OR. <CondB> <B>");
            condB();
            b();
        } else if (matchAny(LexicalUnit.RIGHT_PARENTHESIS)) {
            printRule(34, "B", "\u0395");
        } else {
            throw new ParserException(peeked, 32);
        }
    }

    private void condB() throws ParserException {
        printRule(35, "CondB", "<CondC> <D>");
        if (matchAny(LexicalUnit.NOT, LexicalUnit.VARNAME, LexicalUnit.NUMBER, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS)){
            condC();
        } else {
            throw new ParserException(peeked, 35);
        }
        d();
    }

    private void d() throws ParserException {
        if (match(LexicalUnit.AND)) {
            printRule(36, "D", ".AND. <CondC>");
            condC();
        } else if (matchAny(LexicalUnit.OR, LexicalUnit.RIGHT_PARENTHESIS)) {
            printRule(37, "D", "\u0395");
        } else {
            throw new ParserException(peeked, 35);
        }
    }

    private void condC() throws ParserException {
        if (match(LexicalUnit.NOT)) {
            printRule(38, "CondC", ".NOT. <SimpleCond>");
        } else {
            printRule(39, "CondC", "<SimpleCond>");
        }
        simpleCond();
    }

    private void simpleCond() throws ParserException {
        printRule(40, "SimpleCond", "<ExprArithA> <Comp> <ExprArithA>");
        exprArithA();
        comp();
        exprArithA();
    }

    private void comp() throws ParserException {
        switch (peeked.getType()) {
        case EQUAL_COMPARE:
            printRule(41, "Comp", ".EQ.");
            break;
        case GREATER_EQUAL:
            printRule(42, "Comp", ".GE.");
            break;
        case GREATER:
            printRule(43, "Comp", ".GT.");
            break;
        case SMALLER_EQUAL:
            printRule(44, "Comp", ".LE.");
            break;
        case SMALLER:
            printRule(45, "Comp", ".LT.");
            break;
        case DIFFERENT:
            printRule(46, "Comp", ".NE.");
            break;
        default:
            throw new ParserException(peeked, 40);
        }
        peek();
    }

    private void doRule() throws ParserException {
        printRule(47, "Do", "DO [VarName] = [Number] , [Number] [EndLine] <Code> ENDDO");
        matchOrThrow(LexicalUnit.DO, 47);
        matchOrThrow(LexicalUnit.VARNAME, 47);
        matchOrThrow(LexicalUnit.EQUAL, 47);
        matchOrThrow(LexicalUnit.NUMBER, 47);
        matchOrThrow(LexicalUnit.COMMA, 47);
        matchOrThrow(LexicalUnit.NUMBER, 47);
        matchOrThrow(LexicalUnit.ENDLINE, 47);
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT, LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)){
            code();
        } else {
            throw new ParserException(peeked, 47);
        }
        matchOrThrow(LexicalUnit.ENDDO, 47);
    }

    private void print() throws ParserException {
        printRule(48, "Print", "PRINT* , <ExpList>");
        matchOrThrow(LexicalUnit.PRINT, 48);
        matchOrThrow(LexicalUnit.COMMA, 48);
        expList();
    }

    private void read() throws ParserException {
        printRule(49, "Read", "READ* , <VarList>");
        matchOrThrow(LexicalUnit.READ, 49);
        matchOrThrow(LexicalUnit.COMMA, 49);
        varlist();
    }

    private void expList() throws ParserException {
        printRule(50, "ExpList", "<ExprArithA> <FollowExpList>");
        exprArithA();
        followExplist();
    }

    private void followExplist() throws ParserException {
        if (match(LexicalUnit.COMMA)) {
            printRule(51, "FollowExpList", ", <ExpList>");
            expList();
        } else if (matchAny(LexicalUnit.ENDLINE)) {
            printRule(52, "FollowExpList", "\u0395");
        } else {
            throw new ParserException(peeked, 50);
        }
    }
}