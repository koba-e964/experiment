require './main.rb'
require './parse.rb'
require './eval.rb'


include RbScmTokenize
include RbScmParse
include RbScmEval

global_map=SymMap.new()
RbScmEval::set_global_map(global_map)
RbScmEval::add_initial_operator(global_map)

def run(str)
	tokens=tokenize(str)
	pos=0
	res=nil
	while(pos<tokens.size)
		#p [pos,tokens.size,tokens[pos...tokens.size]]
		sobj,len=parse_expr(tokens[pos...tokens.size])
		res=sobj_eval(sobj)
		pos+=len
	end
	return res
end


def read_file(filename)
	str=''
	fp=open(filename,'r')
	for line in fp
		str+=line
	end
	fp.close
	run(str)
	nil
end
read_file('lib.txt')


while true
	print ">>>"
	line=STDIN.readline
	line.strip!
	if(line=="exit")
		break
	end
	if(line=="env")
		p global_map
		next
	end
	begin
		sexpr,slen=parse_expr(tokenize(line))
		puts sobj_eval_sym(sexpr,global_map)
	rescue => ex
		p ex
		puts ex.backtrace
	end
end
