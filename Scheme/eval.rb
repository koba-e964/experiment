require './main.rb'
module RbScmEval
	def eval(sobj)
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
			func=eval(car)
			puts "call("+func.to_s+", args="+cdr.to_s+")"
		end
	end
end

class SymMap #map: ScmSymbol->SObj
	def initialize()
		@map={} #map
	end
	def add(sym,obj)
		raise unless sym.is_a? ScmSymbol
		raise unless obj.is_a? SObj
		if contains?(sym)
			raise 'this contains '+sym.to_s
		end
		@map[sym]=obj
	end
	def []=(sym,obj)
		raise unless sym.is_a? ScmSymbol
		raise unless obj.is_a? SObj
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
