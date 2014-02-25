require './main.rb'
require './eval.rb'
require './parse.rb'

#test of sobj_eval

#initialization
include RbScmEval
include RbScmTokenize
include RbScmParse

def assert_equal(actual,exp)
	if actual != exp
		puts "expected:"+exp.to_s+", but actual:"+actual.to_s
	end
end

def test_eval(expr,expected)
	actual=run(expr)
	exp_s=ruby_to_SObj(expected)
	if actual != exp_s
		puts "expr:"+expr+", expected:"+exp_s.to_s+", actual:"+actual.to_s
	end
end

gl_map=SymMap.new()
set_global_map(gl_map)
add_initial_operator(gl_map)

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


test_eval '(+ 1 2 3)', 6
test_eval '(if (set! x 3) x 1)', 3

test_eval '(cond ((quote (1 3)) => (lambda (x) x)))', [1,3]

#and or

test_eval '(and #t 2 3 4 5)', 5
test_eval '(or #f ((lambda (x) x) #f) #f)', false

puts "----- 4.2.2 Binding constructs:"
# let, let*, letrec
test_eval '(let ((x 1) (y 2)) (+ x y))', 3
test_eval '(let ((x 1) (y 2)) (let ((x y) (y (+ x 10))) (list x y)))', [2,11]

test_eval '(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))', 70
test_eval '(let ((x 1) (y 2)) (let* ((x y) (y (+ x 10))) (list x y)))', [2,12]

test_eval <<EOS, [true,false,false]
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




#environment
test_eval '(define a 2) ((lambda (a) a)8) a', 2

#improper list
test_eval '(define imp-list-0 (quote (2 . 4))) imp-list-0', [2,'.',4]



test_eval '(+ (abs 3) (abs -100000))', 3+100000

#env test

test_eval <<EOS, 10
(define xx 4)
((lambda (zz) (+ xx zz)) 6)
EOS

test_eval <<EOS, 10
(define xx 4)
(define f14 (lambda (zz) (+ xx zz)))
(begin (set! xx 10) (f14 6))
EOS

test_eval <<EOS, 16
(define xx 4)
(define f14 (lambda (zz) (+ xx zz)))
(define xx 10)
(f14 6)
EOS


puts "----- 6.1 Equivalence predicates:"
assert_equal(run('(eqv? (quote a) (quote a))'), ruby_to_SObj(true))
assert_equal(run('(eqv? (list 1) (list 1))'), ruby_to_SObj(false))

puts "----- 6.2.4 numerical constants:"
test_eval '(quote (#b1010 #o777 #d10000 #xffff))', [10,0777,10000,0xffff]
test_eval '(quote (-100 #x-ffff))', [-100,-0xffff]


puts "----- 6.2.5 Numerical operations:"

# misc
test_eval '(* 3 7 5 6 4)', 3*7*5*6*4
test_eval '(integer? (quote (1 2 3)))', false
test_eval '(integer? (+ 1 2 3))', true

# division
test_eval '(list (quotient 59 7) (quotient -37 13) (quotient 43 -19) (quotient -23 -17))',[8,-2,-2,1]
test_eval '(list (remainder 59 7) (remainder -37 13) (remainder 43 -19) (remainder -23 -17))',[3,-11,5,-6]
test_eval '(list (modulo 59 7) (modulo -37 13) (modulo 43 -19) (modulo -23 -17))',[3,2,-14,-6]

# number theoretic
test_eval '(list (zero? 7) (zero? 0))',[false,true]
test_eval '(list (positive? 7) (positive? 0))',[true,false]
test_eval '(list (negative? 7) (negative? 0))',[false,false]
test_eval '(list (even? 7) (even? -4))',[false,true]
test_eval '(list (odd? -7) (odd? 6))',[true,false]

# comparison
test_eval '(< 2 3 4 5)', true
test_eval '(< -2 6 4 5)', false
test_eval '(= 2 2 2 2)', true
test_eval '(= 2 1 1 1)', false
test_eval '(> 2 1 0 -1)', true
test_eval '(> 2 1 7 5)', false
test_eval '(<= 2 2 2 2)', true
test_eval '(<= 2 1 1 1)', false
test_eval '(>= 2 1 0 -1)', true
test_eval '(>= 2 1 7 5)', false

puts "----- 6.3.2 Pairs and lists:"
test_eval '(pair? 2)',false
test_eval '(pair? (list 1 2))',true
test_eval '(pair? (quote ()))',false

test_eval '(cons (list 2 3) (list 4 5))', [[2,3],4,5]
test_eval '(car (list 2 3 4))', 2
test_eval '(cdr (list 2 3 4))', [3,4]

test_eval '(list? (cons 2 3))',false
test_eval '(list? (cons 2 (cons 3 (list))))', true

test_eval '(length (list 1 2 3 4 5))',5
test_eval '(append (list 1 2 3 4 5) (list 6 7 8) (list 9 10))', [1,2,3,4,5,6,7,8,9,10]
test_eval '(append (list) (quote aaa))', 'aaa'

test_eval '(reverse (list 1 2 3 4 5))', [5,4,3,2,1]
test_eval '(reverse (quote ()))',[]

test_eval '(list +)', "(<+>)"

test_eval '(list-tail (list 1 2 3 4 5) 3)',[4,5]
test_eval '(list-ref (list 1 2 3 4 5) 3)', 4
test_eval '(mem-general = 10 (list 1 4 10 3 2))',[10,3,2]
test_eval '(assoc-general = 10 (quote ((1 4) (10 3) (2 10))))',[10,3]



puts "-----do:"
test_eval '(do ((s ()) (i 0 (+ i 1))) ((= i 5) s) (set! s (cons i s)))',[4,3,2,1,0]

puts "-----eqv?"
test_eval '(begin (define x (list 2 1 0)) (list (eqv? x x) (eqv? x (list 2 1 0))))',[true,false]

puts "-----case"
test_eval '(case (+ 10 5) ((1 10 15) (quote first)) ((2 13 16) (quote second)) (else (quote else)))', "first"
