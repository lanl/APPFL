import Lexer
import Parser

teststring1 = "one = CON(I 1);"
teststring2 = "zero = CON(I 0);\n one = CON(I 1);"
teststring3 = "error = ERROR;\n zero = CON(I 0);\n one = CON(I 1);"
teststring4 = "unit = CON(Unit);"

prelude1 = "error = ERROR;\n unit = CON(Unit);\n true = CON(True);\n false = CON(False);\n nil = CON(Nil);\n zero = CON(I 0);\n one = CON(I 1);\n two = CON(I 2);\n three = CON(I 3);\n four = CON(I 4);\n five = CON(I 5);\n six = CON(I 6);\n seven = CON(I 7);\n eight = CON(I 8);\n nine = CON(I 9);\n ten = CON(I 10);\n"

prelude2 = "plusInt = FUN(x y ->\n case x of {\n I i -> case y of {\n I j -> case plus# i j of {\n x -> let { result = CON (I x) } in result }}});\n"

fun1 = "const = FUN(x y -> x);"
fun2 = "apply = FUN(f x -> f x);"

let1 = "let { result = CON (I x) } in result"

case1 = "case list of {Nil -> nil}"
case2 = "case list of {foo -> nil}"

add1 = "main = THUNK(plus# 1 2);"
add2 = "one = CON(I 1);\n two = CON(I 2);\n plusInt = FUN(x y ->\n case x of {\n I i -> case y of {\n I j -> case plus# i j of {\n x -> let { result = CON (I x) } in result }}});\n main = THUNK(plusInt one two);"

testlex = strip.fst.head.lexer.prelex

testparse = fst.head.program.strip.fst.head.lexer.prelex

testdeclparse = fst.head.declaration.strip.fst.head.lexer.prelex

testexprparse = fst.head.expression.strip.fst.head.lexer.prelex
