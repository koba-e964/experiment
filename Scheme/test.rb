require './main.rb'
require './eval.rb'


#test of  ruby_to_SObj

include RbScm
sobj0=ruby_to_SObj([1,2,[symbol('+'),false]])
puts "sobj0.to_s="+sobj0.to_s


#test og sobj_eval

include RbScmEval

sexp0=ruby_to_SObj([symbol('+'),1,2,[symbol('+'),100]])
#(+ 1 2 (+ 100))
p sobj_eval(sexp0)