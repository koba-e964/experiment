require './main.rb'
require './eval.rb'
require './parse.rb'

#test of  ruby_to_SObj

include RbScm
sobj0=ruby_to_SObj([1,2,[symbol('+'),false]])
puts "sobj0.to_s="+sobj0.to_s


include RbScmTokenize
include RbScmParse

#test of tokenize()
include RbScmTokenize

token0=tokenize(<<EOS)
(define (reverse ls)
  (letrec ((mrs
    (lambda (l1 l2)
      (if (null? l1)
        l2
        (mrs(cdr l1)(cons(car l1)l2))
      )
    )
  )) (mrs ls ()))
)
EOS
p token0
token1=tokenize("`(,@(list 2 3) x p)")
print token1

#test of RbScmParse
include RbScmParse

parse0=parse_expr(tokenize("()"))
p parse0

parse1=parse_expr(tokenize("(1 2 3 +)"))
p parse1

parse2=parse_expr(tokenize("( #(7 8 9)#(11)#())"))
p parse2

parse3=parse_expr(token0)
p parse3

parse4=parse_expr(tokenize("(2 3 4 . 5)"))
p parse4


