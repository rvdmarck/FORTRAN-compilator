class ParserException extends Exception {
    ParserException(Symbol s) {
        super("Unexpected token: " + s.getValue() + " (line " + s.getLine() + ", col " + s.getColumn() + ")");
    }
}
