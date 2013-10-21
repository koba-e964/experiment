require './main.rb'
require './eval.rb'
require './parse.rb'

#test of  ruby_to_SObj

include RbScm
sobj0=ruby_to_SObj([1,2,[symbol('+'),false]])
puts "sobj0.to_s="+sobj0.to_s


#test of sobj_eval

include RbScmEval

sexp0=ruby_to_SObj([symbol('+'),1,2,[symbol('+'),100]])
#(+ 1 2 (+ 100))
p sobj_eval(sexp0)

sexp1=ruby_to_SObj([symbol('-'),2,3,[symbol('-'),100],[symbol('-'),3,2]])
#(- 2 3 (- 100) (- 3 2) ---->98
p sobj_eval(sexp1)

sexp2=ruby_to_SObj([symbol('add2'),5,10])
#(add2 5 10)
p sobj_eval(sexp2)

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
puts token1

#test of RbScmParse

parse0=parse_expr(tokenize("()"))
p parse0

