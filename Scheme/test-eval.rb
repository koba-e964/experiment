require './main.rb'
require './eval.rb'
require './parse.rb'

#test of sobj_eval

#initialization
include RbScmEval
include RbScmTokenize
include RbScmParse

gl_map=SymMap.new()
set_global_map(gl_map)
add_initial_operator(gl_map)

sexp0=ruby_to_SObj([symbol('+'),1,2,[symbol('+'),100]])
#(+ 1 2 (+ 100))
p sobj_eval(sexp0)

sexp1=ruby_to_SObj([symbol('-'),2,3,[symbol('-'),100],[symbol('-'),3,2]])
#(- 2 3 (- 100) (- 3 2) ---->98
p sobj_eval(sexp1)

#eval.rb sobj_eval

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

#environment
result11=run('(define a 2) ((lambda (a) a)8) a')
p [result11,2]

#improper list
result=run('(define imp-list-0 (quote (2 . 4))) imp-list-0')
p [result,[2,'.',4]]


#lib.txt
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

result12=run('(+ (abs 3) (abs -100000))')
p [result12,3+100000]

#env test

result13=run(<<EOS)
(define xx 4)
((lambda (zz) (+ xx zz)) 6)
EOS
p [result13, 10]

result14=run(<<EOS)
(define xx 4)
(define f14 (lambda (zz) (+ xx zz)))
(begin (set! xx 10) (f14 6))
EOS
p [result14, 10]

result15=run(<<EOS)
(define xx 4)
(define f14 (lambda (zz) (+ xx zz)))
(define xx 10)
(f14 6)
EOS
p [result15, 16]

