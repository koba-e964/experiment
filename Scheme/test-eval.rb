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

puts "-----division:"

result=run('(list (quotient 59 7) (quotient -37 13) (quotient 43 -19) (quotient -23 -17))')
p [result,[8,-2,-2,1]]
result=run('(list (remainder 59 7) (remainder -37 13) (remainder 43 -19) (remainder -23 -17))')
p [result,[3,-11,5,-6]]
result=run('(list (modulo 59 7) (modulo -37 13) (modulo 43 -19) (modulo -23 -17))')
p [result,[3,2,-14,-6]]

puts "-----division end"
puts "-----number theoretic:"
p [run('(list (zero? 7) (zero? 0))'),[false,true]]
p [run('(list (positive? 7) (positive? 0))'),[true,false]]
p [run('(list (negative? 7) (negative? 0))'),[false,false]]
p [run('(list (even? 7) (even? -4))'),[false,true]]
p [run('(list (odd? -7) (odd? 6))'),[true,false]]

puts "-----number theoretic end"
puts "-----comparison:"
p [run('(< 2 3 4 5)'), "#t"]
p [run('(< -2 6 4 5)'), "#f"]
p [run('(= 2 2 2 2)'), "#t"]
p [run('(= 2 1 1 1)'), "#f"]
p [run('(> 2 1 0 -1)'), "#t"]
p [run('(> 2 1 7 5)'), "#f"]
p [run('(<= 2 2 2 2)'), "#t"]
p [run('(<= 2 1 1 1)'), "#f"]
p [run('(>= 2 1 0 -1)'), "#t"]
p [run('(>= 2 1 7 5)'), "#f"]
puts "-----comparison end"

puts "-----pairs and lists:"
p [run('(pair? 2)'),false]
p [run('(pair? (list 1 2))'),true]
p [run('(pair? (quote ()))'),false]

p [run('(cons (list 2 3) (list 4 5))'),[[2,3],4,5]]
p [run('(car (list 2 3 4))'),2]
p [run('(cdr (list 2 3 4))'),[3,4]]

p [run('(list? (cons 2 3))'),false]
p [run('(list? (cons 2 (cons 3 (list))))'),true]

p [run('(length (list 1 2 3 4 5))'),5]
p [run('(append (list 1 2 3 4 5) (list 6 7 8) (list 9 10))'),[1,2,3,4,5,6,7,8,9,10]]
p [run('(append (list) (quote aaa))'),'aaa']

p [run('(reverse (list 1 2 3 4 5))'),[5,4,3,2,1]]
p [run('(reverse (quote ()))'),[]]

#this code works correctly
p [run('(list +)'),"(<+>)"]

p [run('(list-tail (list 1 2 3 4 5) 3)'),[4,5]]
p [run('(list-ref (list 1 2 3 4 5) 3)'),4]
p [run('(mem-general = 10 (list 1 4 10 3 2))'),[10,3,2]]
p [run('(assoc-general = 10 (quote ((1 4) (10 3) (2 10))))'),[10,3]]



puts "-----pairs and lists end"


puts "-----let, let* and letrec:"
p [run('(let ((x 1) (y 2)) (+ x y))'),3]
p [run('(let ((x 1) (y 2)) (let ((x y) (y (+ x 10))) (list x y)))'),[2,11]]

p [run('(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))'),70]
p [run('(let ((x 1) (y 2)) (let* ((x y) (y (+ x 10))) (list x y)))'),[2,12]]

p [run(<<EOS),[true,false,false]]
(letrec (
		(e (lambda (n)
			(if (zero? n) #t (o (- n 1)))
		))
		(o (lambda (n)
			(if(zero? n) #f (e (- n 1)))
		))
	)
	(list (e 6) (o 10) (e 7))
)
EOS
puts "-----let, let* and letrec end"
puts "-----do:"
p [run('(do ((s ()) (i 0 (+ i 1))) ((= i 5) s) (set! s (cons i s)))'),[4,3,2,1,0]]
puts "-----do end"

puts "-----eqv?"
p [run('(begin (define x (list 2 1 0)) (list (eqv? x x) (eqv? x (list 2 1 0))))'),[true,false]]

puts "-----case"
p [run('(case (+ 10 5) ((1 10 15) (quote first)) ((2 13 16) (quote second)) (else (quote else)))'),"first"]
