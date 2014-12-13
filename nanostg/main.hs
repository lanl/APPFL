import Lexer
import Parser

teststring1 = "one = CON(I 1);"
teststring2 = "zero = CON(I 0);\n one = CON(I 1);"
teststring3 = "error = ERROR;\n zero = CON(I 0);\n one = CON(I 1);"
teststring4 = "unit = CON(Unit);"

prelude1 = "error = ERROR;\n unit = CON(Unit);\n true = CON(True);\n false = CON(False);\n nil = CON(Nil);\n zero = CON(I 0);\n one = CON(I 1);\n two = CON(I 2);\n three = CON(I 3);\n four = CON(I 4);\n five = CON(I 5);\n six = CON(I 6);\n seven = CON(I 7);\n eight = CON(I 8);\n nine = CON(I 9);\n ten = CON(I 10);\n"

fun1 = "const = FUN(x y -> x);"
fun2 = "apply = FUN(f x -> f x);"


testlex = strip.fst.head.lexer.prelex

testparse = fst.head.program.strip.fst.head.lexer.prelex

testdeclparse = fst.head.declaration.strip.fst.head.lexer.prelex
