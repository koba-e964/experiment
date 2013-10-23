require './main.rb'
require './parse.rb'
require './eval.rb'


include RbScmTokenize
include RbScmParse
include RbScmEval
while true
	print ">>>"
	line=STDIN.readline
	line.strip!
	if(line=="exit")
		break
	end
	begin
		sexpr,slen=parse_expr(tokenize(line))
		puts sobj_eval(sexpr)
	rescue => ex
		p ex
		puts ex.backtrace
	end
end
