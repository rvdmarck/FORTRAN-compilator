class ParserException extends Exception {
    ParserException(Symbol s, int rule) {
        super("Unexpected token: " + s.getValue() + " (line " + s.getLine() + ", col " + s.getColumn() + ", rule [" + rule + "])");
    }
}
