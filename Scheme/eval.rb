require './main.rb'
module RbScmEval
	extend Kind
	include Kind
	extend RbScm
	include RbScm
	
	@@map=nil
	module_function
	def set_global_map(map)
		@@map=map
	end
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
					return make_lambda(cdr,local)
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
				when "let"
					return eval_let(cdr,local)
				when "let*"
					return eval_letstar(cdr,local)
				when "letrec"
					return eval_letrec(cdr,local)
				when "do"
					return eval_do(cdr,local)
				when "case"
					return eval_case(cdr,local)
				when  "delay", "quasiquote"
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
			if func.type==PROC || func.type==LAMBDA
				return (func.data)[args]
			end
			raise 'neiter Proc nor LambdaClosure'
		when INT, STRING, SYNTAX, BOOL
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
		end
		raise Exception
	end
	def make_lambda(cdr,local)
		check_argc(cdr,2,true) #(param expr...)
		param,exprs=pair_divide(cdr)
		inst=LambdaClosure.new(param,exprs,local)
		ret=SObj.new
		ret.set_lambda(inst)
		return ret
	end
	#(varname expr)
	def eval_define(cdr,local) #this method defines global variables. internal definitions are not supported.
		check_argc(cdr,2)
		varname,expr=pair_divide(cdr)
		if varname.type==PAIR # definition of function
			funcname,args=pair_divide(varname)
			#(lambda args expr)
			lambdaexpr=make_pair(make_syntax("lambda"),make_pair(args,expr))
			funcname.type==SYMBOL or raise 'not symbol:'+funcname.to_s
			return eval_define(make_pair(funcname,make_pair(lambdaexpr,make_null)),local) # recursive invocation
		end
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
	def eval_let(cdr,local) #(let ((name init)...) expr), every initial value is evaluated before bindings.
		check_argc(cdr,2,true)
		bindings,exprs=pair_divide(cdr)
		varmap={}
		while bindings.type!=NULL
			b,bindings=pair_divide(bindings)
			check_argc(b,2)
			name,init=pair_divide(b)
			init,_=pair_divide(init)
			if varmap.has_key? name.data
				raise 'duplicate variables in let, name:'+name.data.to_s
			end
			varmap[name.data]=sobj_eval_sym(init,local) #evaluates in given environment
		end
		copy=local.copy
		for k,v in varmap
			copy[k]=v
		end
		return eval_begin(exprs,copy)
	end
	def eval_letstar(cdr,local) #(let* ((name init)...) expr), evaluation of initial values and bindings are done one after another.
		check_argc(cdr,2,true)
		bindings,exprs=pair_divide(cdr)
		copy=local.copy
		#duplicate variable names are allowed.
		while bindings.type!=NULL
			b,bindings=pair_divide(bindings)
			check_argc(b,2)
			name,init=pair_divide(b)
			init,_=pair_divide(init)
			copy[name.data]=sobj_eval_sym(init,copy) #evaluates in copy
		end
		return eval_begin(exprs,copy)
	end
	def eval_letrec(cdr,local) #(letrec ((name init)...) expr), the binding rule is complicated.
		check_argc(cdr,2,true)
		bindings,exprs=pair_divide(cdr)
		copy=local.copy
		blist=[]
		while bindings.type!=NULL
			b,bindings=pair_divide(bindings)
			check_argc(b,2)
			name,init=pair_divide(b)
			init,_=pair_divide(init)
			blist=blist+[[name,init]]
		end
		for name,_ in blist
			copy[name.data]=make_undef()
		end
		for name,init in blist
			copy[name.data]=sobj_eval_sym(init,copy)
		end
		return eval_begin(exprs,copy)
	end
	def eval_do(cdr,local) #( ((var init step)...) (test expr...) cmds...)
		check_argc(cdr,3,true)
		iter,rest1=pair_divide(cdr)
		ending,cmds=pair_divide(rest1)
		copy=local.copy
		stepmap={}
		while iter.type!=NULL
			vis,iter=pair_divide(iter) #var init step->vis
			check_argc(vis,2,true) # 2 or 3
			var,is=pair_divide(vis)
			init,step=pair_divide(is)
			if step.type==NULL #step is not given
				step=var #step has no effect
			else
				step,nulllist=pair_divide(step)
				check_argc(nulllist,0)
			end
			copy[var.data]=sobj_eval_sym(init,local)
			stepmap[var.data]=step
		end
		test,exprs=pair_divide(ending)
		while true
			res=sobj_eval_sym(test,copy)
			if scm_true?(res)
				return eval_begin(exprs,copy)
			end
			#the loop continues
			eval_begin(cmds,copy)
			#steps
			for name,step in stepmap
				copy[name]=sobj_eval_sym(step,copy)
			end
		end
		raise 'unreachable code'
	end
	def list_contains(key,ls)
		while ls.type!=NULL
			a,ls=pair_divide(ls)
			res=sobj_eval(ruby_to_SObj([symbol("eqv?"), key, a]))
			if(scm_true?(res))
				return true
			end
		end
		return false
	end
	# (key ((data...) exprs...)...), data are not evaluated.
	def eval_case(cdr,local)
		check_argc(cdr,1,true) # 1+
		key,rest=pair_divide(cdr)
		key=sobj_eval_sym(key,local)
		while rest.type!=NULL
			de,rest=pair_divide(rest)
			data,exprs=pair_divide(de)
			if((data==make_symbol("else") && rest.type==NULL) ||
				 list_contains(key,data))
				return eval_begin(exprs,local)
			end
		end
		return make_undef()
	end
end

class SymMap #map: ScmSymbol->(SObj or Proc or LambdaClosure)
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
		raise unless obj.is_a?(SObj)||obj.is_a?(Proc)||obj.is_a?(LambdaClosure) || obj.nil?
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
	def to_s
		str=''
		for entry in @map
			str+=entry[0].to_s+' ===> '+
			(if entry[1].is_a?(Proc) then 'Proc' else entry[1].to_s end)+
			"\n"
		end
		return str
	end
	def inspect
		"SymMap:\n"+to_s
	end
	#copies this symmap. Returned instance has no relation with this.
	def copy
		inst=SymMap.new()
		for s,v in @map
			inst[s]=v
		end
		return inst
	end
end
class LambdaClosure
	attr_reader :param,:exprs, :env
	def initialize(param,exprs,env)
		@param=param
		@exprs=exprs
		@env=env
	end
	def [](args) #evaluation
		argcopy=args
		copy=env.copy
		pr=param
		while(pr.type==PAIR)
			pn,pr=pair_divide(pr)
			#pn<-args[?]
			raise 'too few arguments param='+param.to_s+' args='+argcopy.to_s unless args.type==PAIR
			val,args=pair_divide(args)
			copy[pn.data]=val #overwrite if already defined
		end
		if(pr.type!=NULL)
			# pr is symbol (rest parameter)
			pr.type==SYMBOL or raise 'not symbol:'+pr.inspect
			copy[pr.data]=args
		else
			#not variable-length-argument, too many arguments
			args.type==NULL or raise 'too many arguments param='+param.to_s+' args='+argcopy.to_s
		end
		return eval_begin(exprs,copy)
	end
	def to_s
		return "#<closure >"
	end
	def inspect
		return to_s
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
	def reg_proc(sym,proc)
		sobj=SObj.new
		sobj.set_proc(proc)
		@@map[sym]=sobj
	end
	def add_initial_operator(map)
		reg_proc(symbol('+'), lambda{|varargs| #argument is passed as a list
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
		reg_proc(symbol('-'), lambda{|varargs| #argument is passed as a list
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
		reg_proc(symbol('*'), lambda{|varargs| #argument is passed as a list
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
		reg_proc(symbol('null?'), lambda{|varargs|
			check_argc(varargs,1) #argument length must be 1
			car,_=pair_divide(varargs)
			return ruby_to_SObj(car.type==NULL)
		})
		reg_proc(symbol('number?'), lambda{|varargs|
			check_argc(varargs,1) #argument length must be 1
			car,_=pair_divide(varargs)
			return ruby_to_SObj(car.type==INT) #only int is supported
		})
		reg_proc(symbol('integer?'), lambda{|varargs|
			check_argc(varargs,1) #argument length must be 1
			car,_=pair_divide(varargs)
			return ruby_to_SObj(car.type==INT)
		})
		reg_proc(symbol('<'),lambda{|varargs| #argument is passed as a list
			check_argc(varargs,2,true) # argc>=2
			res=true
			cur=nil
			while varargs.type==PAIR
				car,cdr=pair_divide(varargs)
				raise car.to_s+' is not an int' unless car.type==INT
				res=res && (cur.nil? || cur<car.data)
				cur=car.data
				varargs=cdr
			end
			return ruby_to_SObj(res)
		})
		reg_proc(symbol('='), lambda{|varargs| #argument is passed as a list
			check_argc(varargs,2,true) # argc>=2
			res=true
			cur=nil
			while varargs.type==PAIR
				car,cdr=pair_divide(varargs)
				raise car.to_s+' is not an int' unless car.type==INT
				res=res && (cur.nil? || cur==car.data)
				cur=car.data
				varargs=cdr
			end
			return ruby_to_SObj(res)
		})
		reg_proc(symbol('>'), lambda{|varargs| #argument is passed as a list
			check_argc(varargs,2,true) # argc>=2
			res=true
			cur=nil
			while varargs.type==PAIR
				car,cdr=pair_divide(varargs)
				raise car.to_s+' is not an int' unless car.type==INT
				res=res && (cur.nil? || cur>car.data)
				cur=car.data
				varargs=cdr
			end
			return ruby_to_SObj(res)
		})
		reg_proc(symbol('<='), lambda{|varargs| #argument is passed as a list
			check_argc(varargs,2,true) # argc>=2
			res=true
			cur=nil
			while varargs.type==PAIR
				car,cdr=pair_divide(varargs)
				raise car.to_s+' is not an int' unless car.type==INT
				res=res && (cur.nil? || cur<=car.data)
				cur=car.data
				varargs=cdr
			end
			return ruby_to_SObj(res)
		})
		reg_proc(symbol('>='), lambda{|varargs| #argument is passed as a list
			check_argc(varargs,2,true) # argc>=2
			res=true
			cur=nil
			while varargs.type==PAIR
				car,cdr=pair_divide(varargs)
				raise car.to_s+' is not an int' unless car.type==INT
				res=res && (cur.nil? || cur>=car.data)
				cur=car.data
				varargs=cdr
			end
			return ruby_to_SObj(res)
		})
		reg_proc(symbol('quotient'), lambda{|varargs|
			check_argc(varargs,2)
			a1,a2=pair_divide(varargs)
			a2,_=pair_divide(a2)
			a1.type==INT or raise "a1 is not an int:"+a1.inspect
			a2.type==INT or raise "a2 is not an int:"+a2.inspect
			a1=a1.data
			a2=a2.data
			if a2==0
				raise 'attempt to divide by 0:'+a1.to_s+"/0"
			end
			s1=if a1==0 then 0 else a1/a1.abs end
			s2=a2/a2.abs
			return ruby_to_SObj(a1.abs/a2.abs*s1*s2)
		})
		reg_proc(symbol('remainder'), lambda{|varargs|
			check_argc(varargs,2)
			a1,a2=pair_divide(varargs)
			a2,_=pair_divide(a2)
			a1.type==INT or raise "a1 is not an int:"+a1.inspect
			a2.type==INT or raise "a2 is not an int:"+a2.inspect
			a1=a1.data
			a2=a2.data
			if a2==0
				raise 'attempt to divide by 0:'+a1.to_s+"/0"
			end
			s1=if a1==0 then 0 else a1/a1.abs end
			s2=a2/a2.abs
			q=a1.abs/a2.abs*s1*s2
			return ruby_to_SObj(a1-q*a2) #a1.sgn==rem.sgn
		})
		reg_proc(symbol('modulo'), lambda{|varargs|
			check_argc(varargs,2)
			a1,a2=pair_divide(varargs)
			a2,_=pair_divide(a2)
			a1.type==INT or raise "a1 is not an int:"+a1.inspect
			a2.type==INT or raise "a2 is not an int:"+a2.inspect
			a1=a1.data
			a2=a2.data
			if a2==0
				raise 'attempt to divide by 0:'+a1.to_s+"/0"
			end
			return ruby_to_SObj(a1%a2) #a1.sgn==rem.sgn
		})
		#pairs and lists
		reg_proc(symbol('pair?'), lambda{|varargs|
			check_argc(varargs,1)
			ls,_=pair_divide(varargs)
			return ruby_to_SObj(ls.type==PAIR)
		})
		reg_proc(symbol('cons'), lambda{|varargs|
			check_argc(varargs,2)
			obj1,obj2=pair_divide(varargs)
			obj2,_=pair_divide(obj2)
			return make_pair(obj1,obj2)
		})
		reg_proc(symbol('car'), lambda{|varargs|
			check_argc(varargs,1)
			obj,_=pair_divide(varargs)
			car,cdr=pair_divide(obj)
			return car
		})
		reg_proc(symbol('cdr'), lambda{|varargs|
			check_argc(varargs,1)
			obj,_=pair_divide(varargs)
			car,cdr=pair_divide(obj)
			return cdr
		})
		reg_proc(symbol('eqv?'),lambda{|varargs|
			check_argc(varargs,2)
			a,rest=pair_divide(varargs)
			b,_=pair_divide(rest)
			if a.type!=b.type
				return ruby_to_SObj(false)
			end
			case a.type
			when BOOL, SYMBOL, INT, CHAR
				return ruby_to_SObj(a.data==b.data)
			when NULL
				return ruby_to_SObj(true)
			end
			return ruby_to_SObj(a.object_id==b.object_id)
		})
	end
end
