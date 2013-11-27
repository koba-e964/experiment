require './main.rb'
module RbScmEval
	extend Kind
	include Kind
	extend RbScm
	include RbScm
	@@map=nil
	module_function
	def pair_divide(sobj)
		if sobj.type!=PAIR
			raise 'sobj must be a pair, given:'+sobj.to_s
		end
		return [sobj.data[0],sobj.data[1]]
	end
	def define_global(sym,obj)
		@@map[sym]=obj
	end
	def sobj_eval(sobj)
		return sobj_eval_sym(sobj,SymMap.new())
	end
	def sobj_eval_sym(sobj,local)
		if @@map.nil?
			@@map=SymMap.new()
		end
		raise 'not a SObj:'+sobj.inspect unless sobj.is_a? SObj
		case sobj.type
		when NULL
			ret=SObj.new
			ret.set_null
			return ret
		when PAIR
			#sobj is a list
			car=sobj.data[0]
			cdr=sobj.data[1]
			if(car.type==SYNTAX)
				str=car.data.name
				case str
				when "define"
					return eval_define(cdr,local)
				when "define-syntax"
					raise 'unsupported'
				when "quote"
					return pair_divide(cdr)[0]
				when "lambda"
					return make_pair(car,cdr)
				when "if"
					return eval_if(cdr,local)
				when "set!"
					return eval_set(cdr,local)
				when "begin"
					return eval_begin(cdr,local)
				when "cond"
					return eval_cond(cdr,local)
				when "and"
					return eval_and(cdr,local)
				when "or"
					return eval_or(cdr,local)
				when "case","let",
					"let*","letrec","do","delay","quasiquote"
					raise 'unsupported syntax'+str
				end
				raise "Illegal state. Something wrong happened."
			end
			func=sobj_eval_sym(car,local)
			args=[]
			while cdr.type!=NULL
				raise 'improper list' unless cdr.type==PAIR
				args << sobj_eval_sym(cdr.data[0],local)
				cdr=cdr.data[1]
			end
			args=ruby_to_SObj(args) #convert args from array to list
			#puts "call("+car.to_s+' => '+func.to_s+", args="+args.to_s+")"
			if func.is_a? Proc
				return func[args]
			end
			return apply_lambda(func,args,local)
		when INT
			return sobj.clone
		when SYMBOL
			res=local[sobj.data]
			if res.nil?
				res=@@map[sobj.data]
			end
			if res.nil?
				raise "symbol '"+sobj.to_s+"' not found"
			end
			return res
		when SYNTAX
			return sobj.clone
		when BOOL
			return sobj.clone
		end
		raise Exception
	end
	def apply_lambda(lmd,args,local)
		aa,param=pair_divide(lmd)
		raise 'not lambda' unless aa.type==SYNTAX && aa.data==syntax('lambda')
		param,expr=pair_divide(param)
		expr,null_list=pair_divide(expr)
		raise 'too many arguments(required:3):'+null_list.to_s unless null_list.type==NULL
		copy=local.clone
		while(param.type==PAIR)
			pn,param=pair_divide(param)
			#pn<-args[?]
			raise 'too few argument param='+pn.to_s unless args.type==PAIR
			val,args=pair_divide(args)
			copy.add pn.data,val
		end
		if(param.type!=NULL)
			# param is symbol (rest parameter)
			copy[param.data]=args
		end
		return sobj_eval_sym(expr,copy)
	end
	#(varname expr)
	def eval_define(cdr,local) #TODO this method doesn't support (define (func args...) expr)
		check_argc(cdr,2)
		varname,expr=pair_divide(cdr)
		expr,_=pair_divide(expr)
		v=sobj_eval_sym(expr,local)
		varname.type==SYMBOL or raise 'not symbol:'+varname.to_s
		define_global(varname.data,v)
		return varname
	end
	# (cond then else)
	def eval_if(cdr,local)
		cond,t=pair_divide(cdr)
		t,f=pair_divide(t)
		if(f.type!=NULL)
			f,null_list=pair_divide(f)
			null_list.type==NULL or raise '(if cond then else) expected, but got:(if)+'+cdr.to_s
		else
			f=nil
		end
		cond=sobj_eval_sym(cond,local)
		if(scm_false?(cond))
			if f.nil?
				return make_undef()
			end
			return sobj_eval_sym(f,local)
		end
		return sobj_eval_sym(t,local)
	end
	#(varname expr), TODO FIXME this method defines variable if not defined.
	def eval_set(cdr,local)
		varname,expr=pair_divide(cdr)
		expr,null=pair_divide(expr)
		null.type==NULL or raise null.to_s+' is not null'
		varname.type==SYMBOL or raise varname.to_s+' is not a symbol'
		local[varname.data]=sobj_eval_sym(expr,local)
		return make_undef()
	end
	#(expr...)
	def eval_begin(cdr,local)
		res=make_undef()
		while cdr.type!=NULL
			car,cdr=pair_divide(cdr)
			res=sobj_eval_sym(car,local)
		end
		return res
	end
	#((condition expr)...)
	def eval_cond(cdr,local)
		while cdr.type!=NULL
			car,cdr=pair_divide(cdr)
			cond,expr=pair_divide(car)
			b=sobj_eval_sym(cond,local)
			if(scm_true?(b))
				if(expr.type==NULL) #(cond (b))
					return b
				end
				t,_=pair_divide(expr) # t might be '=>'
				if(t.type==SYMBOL && t.data.name=='=>')
					_,expr=pair_divide(expr)
					expr,null=pair_divide(expr)
					null.type==NULL or raise null.inspect+' is not null'
					res=sobj_eval_sym(expr,local)
					return sobj_eval_sym(
						make_list([expr,make_list([make_syntax('quote'),b])]),local
					)
				end
				return eval_begin(expr,local)
			end
		end
		return make_undef()
	end
	#(expr...)
	def eval_and(cdr,local)
		res=ruby_to_SObj(true)
		while scm_true?(res) && cdr.type!=NULL
			car,cdr=pair_divide(cdr)
			res=sobj_eval_sym(car,local)
		end
		return res
	end
	#(expr...)
	def eval_or(cdr,local)
		res=ruby_to_SObj(false)
		while scm_false?(res) && cdr.type!=NULL
			car,cdr=pair_divide(cdr)
			res=sobj_eval_sym(car,local)
		end
		return res
	end
end

class SymMap #map: ScmSymbol->(SObj or Proc)
	def initialize()
		@map={} #map
	end
	def add(sym,obj)
		raise 'not a symbol:'+sym.inspect unless sym.is_a?(ScmSymbol)
		raise unless obj.is_a?(SObj)||obj.is_a?(Proc)
		if contains?(sym)
			raise 'this contains '+sym.to_s
		end
		@map[sym]=obj
	end
	def []=(sym,obj) #assign nil to clear
		raise 'not a symbol:'+sym.inspect unless sym.is_a?(ScmSymbol)
		raise unless obj.is_a?(SObj)||obj.is_a?(Proc)||obj.nil?
		@map[sym]=obj
	end
	def [](sym)
		raise unless sym.is_a? ScmSymbol
		return @map[sym]
	end
	def contains?(sym)
		raise unless sym.is_a? ScmSymbol
		@map[sym]!=nil
	end
	def delete(sym)
		raise unless sym.is_a? ScmSymbol
		@map.delete(sym)
	end
end

module RbScmEval
	module_function
	extend RbScm
	def check_argc(arglist,num,vararg=false)
		cnt=0
		old=arglist
		while arglist.type==PAIR
			_,arglist=pair_divide(arglist)
			cnt+=1
		end
		arglist.type==NULL or raise 'proper list required for arguments of function:'+old.to_s
		(if vararg then cnt>=num else cnt==num end) or raise 'wrong number of arguments:requirement='+num.to_s+' but received='+old.to_s
	end
	def add_initial_operator()
		if @@map.nil?
			@@map=SymMap.new()
		end
		define_global(symbol('+'),lambda{|varargs| #argument is passed as a list
			sum=0
			while varargs.type==PAIR
				car=varargs.data[0]
				cdr=varargs.data[1]
				raise car.to_s+' is not an int' unless car.type==INT
				sum+=car.data
				varargs=cdr
			end
			raise 'the terminal of list must be ()' unless varargs.type==NULL
			return ruby_to_SObj(sum)
		})
		define_global(symbol('-'),lambda{|varargs| #argument is passed as a list
			#(>= (length varargs) 1)
			check_argc(varargs,1,true)
			sum=0
			car,cdr=pair_divide(varargs)
			raise 'int required, but given:'+car.to_s unless car.type==INT
			if(cdr.type==NULL)
				return ruby_to_SObj(-car.data)
			end
			sum=car.data
			while cdr.type==PAIR
				car,cdr=pair_divide(cdr)
				raise car.to_s+' is not an int' unless car.type==INT
				sum-=car.data
			end
			raise 'the terminal of list must be ()' unless cdr.type==NULL
			return ruby_to_SObj(sum)
		})
		define_global(symbol('*'),lambda{|varargs| #argument is passed as a list
			sum=1
			while varargs.type==PAIR
				car=varargs.data[0]
				cdr=varargs.data[1]
				raise car.to_s+' is not an int' unless car.type==INT
				sum*=car.data
				varargs=cdr
			end
			raise 'the terminal of list must be ()' unless varargs.type==NULL
			return ruby_to_SObj(sum)
		})
		define_global(symbol('null?'),lambda{|varargs|
			check_argc(varargs,1) #argument length must be 1
			car,_=pair_divide(varargs)
			return ruby_to_SObj(car.type==NULL)
		})
		define_global(symbol('number?'),lambda{|varargs|
			check_argc(varargs,1) #argument length must be 1
			car,_=pair_divide(varargs)
			return ruby_to_SObj(car.type==INT) #only int is supported
		})
		define_global(symbol('integer?'),lambda{|varargs|
			check_argc(varargs,1) #argument length must be 1
			car,_=pair_divide(varargs)
			return ruby_to_SObj(car.type==INT)
		})
		define_global(symbol('<'),lambda{|varargs| #argument is passed as a list
			check_argc(varargs,2,true) # argc>=2
			res=true
			cur=nil
			while varargs.type==PAIR
				car=varargs.data[0]
				cdr=varargs.data[1]
				raise car.to_s+' is not an int' unless car.type==INT
				res&=(cur.nil? || cur<car.data)
				cur=car.data
				varargs=cdr
			end
			return ruby_to_SObj(res)
		})
	end
end
RbScmEval::add_initial_operator()
