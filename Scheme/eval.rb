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
		raise unless sobj.is_a? SObj
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
				when "quote"
					return pair_divide(cdr)[0]
				when "lambda"
					return make_pair(car,cdr)
				when "if"
					return eval_if(cdr,local)
				when "set!","begin","cond","and","or","case","let",
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
	# ((variables...) expr)
	def eval_lambda(cdr)
		
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
			sum=0
			if(varargs.type==NULL)
				raise '- requires at least 1 argument'
			end
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
	end
end
RbScmEval::add_initial_operator()
