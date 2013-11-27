require './main.rb'
require './eval.rb'
require './parse.rb'

#test of  ruby_to_SObj

include RbScm
sobj0=ruby_to_SObj([1,2,[symbol('+'),false]])
puts "sobj0.to_s="+sobj0.to_s


#test of sobj_eval

#initialization
include RbScmEval
gl_map=SymMap.new()
set_global_map(gl_map)
add_initial_operator(gl_map)

include RbScmEval

sexp0=ruby_to_SObj([symbol('+'),1,2,[symbol('+'),100]])
#(+ 1 2 (+ 100))
p sobj_eval(sexp0)

sexp1=ruby_to_SObj([symbol('-'),2,3,[symbol('-'),100],[symbol('-'),3,2]])
#(- 2 3 (- 100) (- 3 2) ---->98
p sobj_eval(sexp1)

include RbScmTokenize
include RbScmParse
sfunc0,slen0=parse_expr(tokenize('(lambda (a b) (+ a b))'))
define_global(symbol('add2'),sfunc0)
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


#eval.rb sobj_eval

def run(str)
	sobj_eval(parse_expr(tokenize(str))[0])
end

result0=sobj_eval(parse_expr(tokenize('(+ 1 2 3)'))[0])
p [result0,"=6"]
result1=sobj_eval(parse_expr(tokenize('(if (set! x 3) x 1)'))[0])
p [result1,"=3"]

result2=sobj_eval(parse_expr(tokenize('(cond ((quote (1 3)) => (lambda (x) x)))'))[0])
p [result2,"=(1 3)"]

#and or

result3=run('(and #t 2 3 4 5)')
p [result3,'=5']
result4=run('(or #f ((lambda (x) x) #f) #f)')
p [result4,'=#f']

#6.2.4 numerical constants
result5=run('(quote (#b1010 #o777 #d10000 #xffff))')
p [result5,[10,0777,10000,0xffff]]

result5_1=run('(quote (-100 #x-ffff))')
p [result5_1,[-100,-0xffff]]

#6.2.5 numerical operations

result6=run('(* 3 7 5 6 4)')
p [result6,3*7*5*6*4]

result7=run('(< 2 3 4 5)')
p [result7, "#t"]

result8=run('(< -2 6 4 5)')
p [result8, "#f"]

result9=run('(integer? (quote (1 2 3)))')
p [result9,"#f"]


result10=run('(integer? (+ 1 2 3))')
p [result10,"#t"]


