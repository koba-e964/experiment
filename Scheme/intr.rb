require './main.rb'
require './parse.rb'
require './eval.rb'


include RbScmTokenize
include RbScmParse
include RbScmEval

global_map=SymMap.new()
RbScmEval::set_global_map(global_map)
RbScmEval::add_initial_operator(global_map)


while true
	print ">>>"
	line=STDIN.readline
	line.strip!
	if(line=="exit")
		break
	end
	begin
		sexpr,slen=parse_expr(tokenize(line))
		puts sobj_eval_sym(sexpr,global_map)
		p global_map
	rescue => ex
		p ex
		puts ex.backtrace
	end
end
