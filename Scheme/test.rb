require './main.rb'
require './eval.rb'


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

