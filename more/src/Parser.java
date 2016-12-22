import java.io.*;
import java.util.*;


class Parser {
    private LexicalAnalyzer la;
    private Symbol peeked = null;
    private static Set<String> variables = new HashSet<>();
    private static int lastID = -1;
    private static List<Symbol> tmpSymbolList = new ArrayList<Symbol>();
    private static List<String> tmpPrintSymbolList = new ArrayList<String>();
    private static String LLVMFilePath;
    private static int loopID = 0;
    private static int ifID = 0;
    private static Stack<String> tempStack = new Stack<>();
    private static Stack<Integer> ifIdStack = new Stack<>();



    /**
     * Uber-Fortran Parser
     *
     * @param la LexicalAnalyzer containing code scan
     */
    Parser(LexicalAnalyzer la, String srcFilePath) throws CompilationException {
        this.la = la;
        LLVMFilePath = generateLLVMFilePath(srcFilePath);
        try(BufferedWriter bw = new BufferedWriter(new FileWriter(LLVMFilePath))){
            bw.write("");
        } catch(IOException e) {
            System.err.println("Could not open output file !");
        }
    }

    private String generateLLVMFilePath(String srcFilePath){
        int lastDotIndex = srcFilePath.lastIndexOf(".");
        String tmpLLVMFilePath = srcFilePath.substring(0, lastDotIndex+1);
        return tmpLLVMFilePath+"ll";
    }

    private void writeLLVM(String content){
        try(BufferedWriter bw = new BufferedWriter(new FileWriter(LLVMFilePath, true))){
            bw.write(content);
        } catch(IOException e) {
            System.err.println("Could not open output file !");
        }

    }

    private static void create(Symbol varname) throws CompilationException {
        final String privateName = "_" + varname.getValue();
        boolean added = variables.add(privateName);
        if (!added)
            throw new CompilationException("Already declared " + varname);
    }

    private static boolean check(Symbol varname) throws CompilationException {
        final String privateName = "_" + varname.getValue();
        if (!variables.contains(privateName)) {
            throw new CompilationException("Undeclared " + varname.getValue());
        }
        return true;
    }

    private static String nextVariable(){
        return (++lastID)+"";
    }

    private static void evaluateArith() {
        String e2 = tempStack.pop(), op = tempStack.pop(), e1 = tempStack.pop();
        String newID = "%" + nextVariable();
        System.out.println("\t\t" + newID + " = " + op + " i32 " + e1 + ", " + e2);
        tempStack.push(newID);
    }

    private static void evaluateComp() {
        String e2 = tempStack.pop(), op = tempStack.pop(), e1 = tempStack.pop();
        String newID = "%" + nextVariable();
        System.out.println("\t\t" + newID + " = icmp " + op + " i32 " + e1 + ", " + e2);
        tempStack.push(newID);
    }

    private static void evaluateCond() {
        String e2 = tempStack.pop(), op = tempStack.pop(), e1 = tempStack.pop();
        String newID = "%" + nextVariable();
        System.out.println("\t\t" + newID + " = " + op + " i1 " + e1 + ", " + e2);
        tempStack.push(newID);
    }

    /**
     * run the Parser
     *
     * @throws ParserException
     */
    void run() throws ParserException, CompilationException {
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
     *
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
     *
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
     *
     * @param lu   lexical unit to check
     * @param rule rule number if it fails
     * @return true if it match
     * @throws ParserException
     */
    private boolean matchOrThrow(LexicalUnit lu, int rule) throws ParserException, CompilationException {
        if (!match(lu))
            throw new ParserException(peeked, rule);
        else
            return true;
    }

    /**
     * pretty printer for rules
     *
     * @param n     rule number
     * @param left  part in the left of arrow
     * @param right part in the right of arrow
     */
    private void printRule(int n, String left, String right) {
        String nStr = "[" + n + "]";
        left = "<" + left + ">";
        System.out.println(String.format("%-5s %-15s \u2192 %s", nStr, left, right));
    }

    private void program() throws ParserException, CompilationException {
        //printRule(1, "Program", "PROGRAM [ProgName] [EndLine] <Vars> <Code> END");
        System.out.println("@formatString = constant [4 x i8] c\"%d\\0A\\00\"\ndeclare i32 @getchar()\ndeclare i32 @printf(i8*,...)");
        System.out.println("define i32 @readInt(){\n" +
                "\tentry:\n" +
                "\t\t%msg = getelementptr inbounds [4 x i8], [4 x i8]* @formatString, i32 0, i32 0\n" +
                "\t\t%res = alloca i32\n" +
                "\t\t%digit = alloca i32\n" +
                "\t\tstore i32 0, i32* %res\n" +
                "\t\tbr label %read\n" +
                "\tread:\n" +
                "\t\t%char = call i32 @getchar()\n" +
                "\t\t%num = sub i32 %char, 48\n" +
                "\t\tstore i32 %num, i32* %digit\n" +
                "\t\t%comp1 = icmp sle i32 0, %num\n" +
                "\t\t%comp2 = icmp sge i32 9, %num\n" +
                "\t\t%comp3 = and i1 %comp1, %comp2\n" +
                "\t\t%comp = icmp eq i1 %comp3, 1\n" +
                "\t\tbr i1 %comp, label %save, label %exit\n" +
                "\tsave:\n" +
                "\t\t%0 = load i32, i32* %res\n" +
                "\t\t%1 = load i32, i32* %digit\n" +
                "\t\t%2 = mul i32 %0, 10\n" +
                "\t\t%3 = add i32 %2, %1\n" +
                "\t\tstore i32 %3, i32* %res\n" +
                "\t\tbr label %read\n" +
                "\texit:\n" +
                "\t\t%ex = load i32, i32* %res\n" +
                "\t\tret i32 %ex\n" +
                "}");
        System.out.println("define void @main(){\n\tentry:\n\t\t%msg = getelementptr inbounds [4 x i8], [4 x i8]* @formatString, i32 0, i32 0");
        matchOrThrow(LexicalUnit.PROGRAM, 1);
        matchOrThrow(LexicalUnit.VARNAME, 1);
        matchOrThrow(LexicalUnit.ENDLINE, 1);
        vars();
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT, LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)) {
            code();
        } else {
            throw new ParserException(peeked, 1);
        }
        matchOrThrow(LexicalUnit.END, 1);
        System.out.println("\t\tret void\n}");
    }

    private void vars() throws ParserException, CompilationException {
        if (match(LexicalUnit.INTEGER)) {
            //printRule(2, "Vars", "INTEGER <VarList> [EndLine]");
            varlist();
            for(Symbol s: tmpSymbolList){
                create(s);
                System.out.println("\t\t%_" + s.getValue() + " = alloca i32");
            }
            tmpSymbolList.clear();
            matchOrThrow(LexicalUnit.ENDLINE, 2);
        } else if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)) {
            //printRule(3, "Vars", "\u0395");
        } else {
            throw new ParserException(peeked, 1);
        }
    }

    private void varlist() throws ParserException, CompilationException {
        //printRule(4, "VarList", "[VarName] <FollowVarList>");
        Symbol tmp = peeked;
        matchOrThrow(LexicalUnit.VARNAME, 4);
        tmpSymbolList.add(tmp);
        followVarlist();
    }

    private void followVarlist() throws ParserException, CompilationException {
        if (match(LexicalUnit.COMMA)) {
            //printRule(5, "FollowVarList", ", <VarList>");
            varlist();
        } else if (matchAny(LexicalUnit.ENDLINE)) {
            //printRule(6, "FollowVarList", "\u0395");
        } else {
            throw new ParserException(peeked, 4);
        }
    }

    private void code() throws ParserException, CompilationException {
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT)) {
           //printRule(7, "Code", "<Instruction> [EndLine] <Code>");
            instruction();
            matchOrThrow(LexicalUnit.ENDLINE, 7);
            code();
        } else if (matchAny(LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)) {
           //printRule(8, "Code", "\u0395");
        } else {
            throw new ParserException(peeked, 7);
        }
    }

    private void instruction() throws ParserException, CompilationException {
        switch (peeked.getType()) {
            case VARNAME:
               //printRule(9, "Instruction", "<Assign>");
                assign();
                break;
            case IF:
               //printRule(10, "Instruction", "<If>");
                ifrule();
                break;
            case DO:
               //printRule(11, "Instruction", "<Do>");
                doRule();
                break;
            case PRINT:
               //printRule(12, "Instruction", "<Print>");
                print();
                break;
            case READ:
               //printRule(13, "Instruction", "<Read>");
                read();
                break;
            default:
                throw new ParserException(peeked, 7);
        }
    }


    private void assign() throws ParserException, CompilationException {
        //printRule(14, "Assign", "[VarName] = <ExprArithA>");
        Symbol varname = peeked;
        matchOrThrow(LexicalUnit.VARNAME, 14);
        check(varname);
        matchOrThrow(LexicalUnit.EQUAL, 14);
        exprArithA();
        String tmp = tempStack.pop();
        System.out.println("\t\tstore i32 " + tmp + ", i32* %_" + varname.getValue());
    }

    private void exprArithA() throws ParserException, CompilationException {
       //printRule(15, "ExprArithA", "<ExprArithB> <V>");
        exprArithB();
        v();
    }

    private void v() throws ParserException, CompilationException {
        if (matchAny(LexicalUnit.PLUS, LexicalUnit.MINUS)) {
           //printRule(16, "V", "<AddOp> <ExprArithB> <V>");
            addOp();
            exprArithB();
            evaluateArith();
            v();
        } else if (matchAny(LexicalUnit.COMMA, LexicalUnit.EQUAL_COMPARE, LexicalUnit.GREATER_EQUAL, LexicalUnit.GREATER, LexicalUnit.SMALLER_EQUAL, LexicalUnit.SMALLER,
                LexicalUnit.DIFFERENT, LexicalUnit.RIGHT_PARENTHESIS, LexicalUnit.TIMES, LexicalUnit.DIVIDE, LexicalUnit.ENDLINE,
                LexicalUnit.AND, LexicalUnit.OR)) {
           //printRule(17, "V", "\u0395");
        } else {
            throw new ParserException(peeked, 15);
        }
    }

    private void exprArithB() throws ParserException, CompilationException {
        //printRule(18, "ExprArithB", "<ExprArithC> <X>");
        exprArithC();
        x();
    }

    private void x() throws ParserException, CompilationException {
        if (matchAny(LexicalUnit.TIMES, LexicalUnit.DIVIDE)) {
            //printRule(19, "X", "<MulOp> <ExprArithC> <X>");
            mulOp();
            exprArithC();
            evaluateArith();
            x();
        } else if (matchAny(LexicalUnit.COMMA, LexicalUnit.EQUAL_COMPARE, LexicalUnit.GREATER_EQUAL, LexicalUnit.GREATER, LexicalUnit.SMALLER_EQUAL, LexicalUnit.SMALLER,
                LexicalUnit.DIFFERENT, LexicalUnit.RIGHT_PARENTHESIS, LexicalUnit.ENDLINE, LexicalUnit.AND, LexicalUnit.OR,
                LexicalUnit.PLUS, LexicalUnit.MINUS)) {
            //printRule(20, "X", "\u0395");
        } else {
            throw new ParserException(peeked, 18);
        }
    }

    private void exprArithC() throws ParserException, CompilationException {
        Symbol s = peeked;
        if (match(LexicalUnit.VARNAME)) {
            //printRule(21, "ExprArithC", "[VarName]");
            check(s);
            String newID = "%" + nextVariable();
            System.out.println("\t\t" + newID + " = load i32, i32* %_" + s.getValue());
            tempStack.push(newID);
        } else if (match(LexicalUnit.NUMBER)) {
            //printRule(22, "ExprArithC", "[Number]");
            tempStack.push((String)s.getValue());
        } else if (match(LexicalUnit.LEFT_PARENTHESIS)) {
            //printRule(23, "ExprArithC", "( <ExprArithA> )");
            exprArithA();
            matchOrThrow(LexicalUnit.RIGHT_PARENTHESIS, 23);
        } else if (match(LexicalUnit.MINUS)) {
            //printRule(24, "ExprArithC", "- <ExprArithC>");
            tempStack.push("0");
            tempStack.push("sub");
            exprArithC();
            evaluateArith();
        } else {
            throw new ParserException(peeked, 18);
        }
    }

    private void addOp() throws ParserException, CompilationException {
        Symbol op = peeked;
        if (match(LexicalUnit.PLUS)) {
            //printRule(25, "AddOp", "+");
        } else if (match(LexicalUnit.MINUS)) {
            //printRule(26, "AddOp", "-");
        } else {
            throw new ParserException(peeked, 16);
        }
        tempStack.push((String)op.getValue());
    }

    private void mulOp() throws ParserException, CompilationException {
        Symbol op = peeked;
        if (match(LexicalUnit.TIMES)) {
            //printRule(27, "MulOp", "*");
        } else if (match(LexicalUnit.DIVIDE)) {
            //printRule(28, "MulOp", "/");
        } else {
            throw new ParserException(peeked, 19);
        }
        tempStack.push((String)op.getValue());
    }

    private void ifrule() throws ParserException, CompilationException {
       //printRule(29, "If", "IF ( <CondA> ) THEN [EndLine] <Code> <IfSeq>");
        matchOrThrow(LexicalUnit.IF, 29);
        ifIdStack.push(ifID++);
        matchOrThrow(LexicalUnit.LEFT_PARENTHESIS, 29);
        condA();
        System.out.println("\t\tbr i1 "+ tempStack.peek() +", label %If" + ifIdStack.peek() + ", label %Else" + ifIdStack.peek());
        System.out.println("\t" + "If" + ifIdStack.peek() + ":");

        matchOrThrow(LexicalUnit.RIGHT_PARENTHESIS, 29);
        matchOrThrow(LexicalUnit.THEN, 29);
        matchOrThrow(LexicalUnit.ENDLINE, 29);
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT, LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)) {
            code();
            System.out.println("\t\tbr label %Endif" + ifIdStack.peek());
        } else {
            throw new ParserException(peeked, 29);
        }
        ifSeq();
        System.out.println("\tEndif" + ifIdStack.peek() + ":");
        ifIdStack.pop();
    }

    private void ifSeq() throws ParserException, CompilationException {
        if (match(LexicalUnit.ENDIF)) {
           //printRule(30, "Else", "ENDIF");
        } else if (match(LexicalUnit.ELSE)) {
            System.out.println("\t" + "Else" + ifIdStack.peek() + ":");
           //printRule(31, "Else", "ELSE [EndLine] <Code> ENDIF");
            matchOrThrow(LexicalUnit.ENDLINE, 31);
            if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT, LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                    LexicalUnit.ENDIF)) {
                code();
                System.out.println("\t\tbr label %Endif" + ifIdStack.peek());
            } else {
                throw new ParserException(peeked, 31);
            }
            matchOrThrow(LexicalUnit.ENDIF, 31);
        } else {
            throw new ParserException(peeked, 29);
        }
    }

    private void condA() throws ParserException, CompilationException {
       //printRule(32, "CondA", "<CondB> <B>");
        condB();
        b();
    }

    private void b() throws ParserException, CompilationException {
        if (match(LexicalUnit.OR)) {
           //printRule(33, "B", ".OR. <CondB> <B>");
            tempStack.push("or");
            condB();
            evaluateCond();
            b();
        } else if (matchAny(LexicalUnit.RIGHT_PARENTHESIS)) {
           //printRule(34, "B", "\u0395");
        } else {
            throw new ParserException(peeked, 32);
        }
    }

    private void condB() throws ParserException, CompilationException {
       //printRule(35, "CondB", "<CondC> <D>");
        if (matchAny(LexicalUnit.NOT, LexicalUnit.VARNAME, LexicalUnit.NUMBER, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS)) {
            condC();
        } else {
            throw new ParserException(peeked, 35);
        }
        d();
    }

    private void d() throws ParserException, CompilationException {
        if (match(LexicalUnit.AND)) {
           //printRule(36, "D", ".AND. <CondC>");
            tempStack.push("and");
            condC();
            evaluateCond();
        } else if (matchAny(LexicalUnit.OR, LexicalUnit.RIGHT_PARENTHESIS)) {
           //printRule(37, "D", "\u0395");
        } else {
            throw new ParserException(peeked, 35);
        }
    }

    private void condC() throws ParserException, CompilationException {
        if (match(LexicalUnit.NOT)) {
           //printRule(38, "CondC", ".NOT. <SimpleCond>");
            tempStack.push("1");
            tempStack.push("xor");
            simpleCond();
            evaluateCond();
        } else {
            //printRule(39, "CondC", "<SimpleCond>");
            simpleCond();
        }
    }

    private void simpleCond() throws ParserException, CompilationException {
       //printRule(40, "SimpleCond", "<ExprArithA> <Comp> <ExprArithA>");
        exprArithA();
        comp();
        exprArithA();

        evaluateComp();
    }

    private void comp() throws ParserException, CompilationException {
        switch (peeked.getType()) {
            case EQUAL_COMPARE:
                //printRule(41, "Comp", ".EQ.");
                break;
            case GREATER_EQUAL:
                //printRule(42, "Comp", ".GE.");
                break;
            case GREATER:
                //printRule(43, "Comp", ".GT.");
                break;
            case SMALLER_EQUAL:
                //printRule(44, "Comp", ".LE.");
                break;
            case SMALLER:
                //printRule(45, "Comp", ".LT.");
                break;
            case DIFFERENT:
                //printRule(46, "Comp", ".NE.");
                break;
            default:
                throw new ParserException(peeked, 40);
        }
        tempStack.push((String)peeked.getValue());
        peek();
    }

    private void doRule() throws ParserException, CompilationException {
       //printRule(47, "Do", "DO [VarName] = [Number] , [Number] [EndLine] <Code> ENDDO");
        matchOrThrow(LexicalUnit.DO, 47);
        matchOrThrow(LexicalUnit.VARNAME, 47);
        matchOrThrow(LexicalUnit.EQUAL, 47);

        int startDO = Integer.parseInt((String)peeked.getValue());
        String newID = "%loop" + loopID;
        loopID++;
        System.out.println("\t\t" + newID + "count = alloca i32");
        System.out.println("\t\tstore i32 " + startDO + ", i32* " + newID + "count");
        System.out.println("\t\tbr label "+ newID);
        System.out.println("\t" + newID.substring(1)+":");

        matchOrThrow(LexicalUnit.NUMBER, 47);
        matchOrThrow(LexicalUnit.COMMA, 47);
        int endDO = Integer.parseInt((String)peeked.getValue());
        String newID__ = "%" + nextVariable();
        String newID_ = "%" + nextVariable();
        System.out.println("\t\t" + newID__ + " = load i32, i32* " + newID + "count");
        System.out.println("\t\t" + newID_ + " = icmp eq i32 " + newID__ + "," +  endDO);
        System.out.println("\t\tbr i1 " + newID_ + ", label %end" + newID.substring(1) +", label %continue"+newID.substring(1));
        System.out.println("\tcontinue"+newID.substring(1)+":");
        String increment = "%" + nextVariable();
        System.out.println("\t\t" + increment + " = load i32, i32* " + newID + "count");
        String increment2 = "%" + nextVariable();
        System.out.println("\t\t" + increment2 + " = add i32 1, " + increment);
        System.out.println("\t\tstore i32 " + increment2 + ", i32* " + newID + "count");
        matchOrThrow(LexicalUnit.NUMBER, 47);
        matchOrThrow(LexicalUnit.ENDLINE, 47);
        if (matchAny(LexicalUnit.VARNAME, LexicalUnit.DO, LexicalUnit.READ, LexicalUnit.IF, LexicalUnit.PRINT, LexicalUnit.ENDDO, LexicalUnit.LEFT_PARENTHESIS, LexicalUnit.MINUS, LexicalUnit.ELSE, LexicalUnit.END,
                LexicalUnit.ENDIF)) {
            code();
            System.out.println("\t\tbr label "+newID);
        } else {
            throw new ParserException(peeked, 47);
        }
        matchOrThrow(LexicalUnit.ENDDO, 47);
        System.out.println("\tend" + newID.substring(1)+":");
    }

    private void print() throws ParserException, CompilationException {
       //printRule(48, "Print", "PRINT* , <ExpList>");
        matchOrThrow(LexicalUnit.PRINT, 48);
        matchOrThrow(LexicalUnit.COMMA, 48);
        expList();
        for(String s: tmpPrintSymbolList){
            String tmp = nextVariable();
            System.out.println("\t\t%" + tmp + " = call i32(i8*,...) @printf(i8* %msg, i32 "+ s +")");
        }
        tmpPrintSymbolList.clear();
    }

    private void read() throws ParserException, CompilationException {
       //printRule(49, "Read", "READ* , <VarList>");

        matchOrThrow(LexicalUnit.READ, 49);
        matchOrThrow(LexicalUnit.COMMA, 49);
        varlist();
        for(Symbol s: tmpSymbolList){
            String tmp = nextVariable();
            if(check(s)) {
                System.out.println("\t\t%" + tmp + "= call i32 @readInt()" +
                        "\n\t\tstore i32 %" + tmp + ", i32* %_" + s.getValue());
            }
        }
        tmpSymbolList.clear();
    }

    private void expList() throws ParserException, CompilationException {
       //printRule(50, "ExpList", "<ExprArithA> <FollowExpList>");
        exprArithA();
        tmpPrintSymbolList.add(tempStack.peek());
        followExplist();
    }

    private void followExplist() throws ParserException, CompilationException {
        if (match(LexicalUnit.COMMA)) {
           //printRule(51, "FollowExpList", ", <ExpList>");
            expList();
        } else if (matchAny(LexicalUnit.ENDLINE)) {
           //printRule(52, "FollowExpList", "\u0395");
        } else {
            throw new ParserException(peeked, 50);
        }
    }
}