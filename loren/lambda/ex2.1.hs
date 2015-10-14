1.) Identify the innermost and outermost beta-redexes in the following lambda expression and draw its abstract syntax tree.

(\x y z -> (add x (mul y z))) ((\x -> (succ x)) 5) 12 ((\w -> (w 4)) sqr)

innermost: ((\x -> (succ x)) 5), ((\w -> (w 4)) sqr)

outermost: (\x y z -> (add x (mul y z))) ((\x -> (succ x)) 5), ((\w -> (w 4)) sqr)
                             
                                   comb
                               /            \
                           /                   \
                        /                         \
                     /                                \
                   comb                                        comb
                 /              \                             /    \ 
               lamb              comb                      lamb    sqr
               /  \              /    \                           /  \ 
              x   lamb          lamb   5                         w   comb 
                  /   \         /  \                                  / \
                 y    lamb     x   comb                              w   4   
                      /  \          /  \
                     z   comb     succ  x
                         / \
                       add comb
                           /  \
                          x   comb
                              /  \
                             mul  comb  
                                  /  \
                                 y    z

