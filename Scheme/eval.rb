require './main.rb'
module RbScmEval
	extend Kind
	include Kind
	extend RbScm
	include RbScm
	@@map=nil
	def sobj_eval(sobj)
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
			func=sobj_eval(car)
			args=[]
			while cdr.type!=NULL
				raise 'improper list' unless cdr.type==PAIR
				args << sobj_eval(cdr.data[0])
				cdr=cdr.data[1]
			end
			args=ruby_to_SObj(args) #convert args from array to list
			puts "call("+func.to_s+", args="+args.to_s+")"
			if func.is_a? Proc
				return func[args]
			end
			raise 'not implemented 25'
			return nil
		when INT
			return sobj.clone
		when SYMBOL
			res=@@map[sobj.data]
			if res.nil?
				raise "symbol '"+sobj.to_s+"' not found"
			end
			return res
		end
		raise Exception
	end
end

class SymMap #map: ScmSymbol->(SObj or Proc)
	def initialize()
		@map={} #map
	end
	def add(sym,obj)
		raise unless sym.is_a? ScmSymbol
		raise unless obj.is_a?(SObj)||obj.is_a?(Proc)
		if contains?(sym)
			raise 'this contains '+sym.to_s
		end
		@map[sym]=obj
	end
	def []=(sym,obj)
		raise unless sym.is_a? ScmSymbol
		raise unless obj.is_a?(SObj)||obj.is_a?(Proc)
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
	def add_initial_operator()
		if @@map.nil?
			@@map=SymMap.new()
		end
		@@map[symbol('+')]=lambda{|varargs| #argument is passed as a list
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
		}
	end
	add_initial_operator()
end