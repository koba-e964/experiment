module Kind
	BOOL=1
	INT=2
	STRING=3
	CHAR=4
	PAIR=5
	NULL=6
	SYMBOL=7
end

class ScmSymbol
	attr_accessor :name
	@@charset=('0'.upto('9').reduce(:+))+('A'.upto('Z').reduce(:+))+('a'.upto('z').reduce(:+))
	@@charset+="!$%&*+-./:<=>?@^_~"
	@@initial=@@charset.clone.delete('0'.upto('9').reduce(:+))
	def initialize(name)
		raise unless ScmSymbol::valid?(name)
		@name=name.downcase
	end
	def self.valid?(name)
		return false unless name.size()>=1
		return false if @@initial.index(name[0]).nil?
		for i in 1...name.size()
			if(@@charset.index(name[i]).nil?)
				return false
			end
		end
		return true
	end
end

module RbScm
	def symbol(name)
		return ScmSymbol.new(name)
	end
end

class SObj
	attr_accessor :type, :data
	extend Kind
	include Kind
	def initialize()
		@type=0
		@data=nil
	end
	def set_bool(val)
		@type=BOOL
		@data=if val then true else false end
	end
	def set_int(val)
		@type=INT
		@data=val
	end
	def set_str(val)
		@type=STRING
		@data=val
	end
	def set_char(val)
		@type=CHAR
		@data=val
	end
	def set_pair(car,cdr)
		@type=PAIR
		@data=[car,cdr]
	end
	def set_null()
		@type=NULL
		@data=nil
	end
	def set_list(val)
		@type=SYMBOL
		@data=val
	end
	def to_s()
		if type==NULL
			return "()"
		end
		if type==PAIR
			if(data[1].type==PAIR)
				tmp=data[1].to_s()
				tmp=tmp[1..tmp.size()-1]
				return "("+data[0].to_s()+" "+tmp
			end
			if(data[1].type==NULL)
				return "("+data[0].to_s()+")"
			end
			return "("+data[0].to_s()+" . "+data[1].to_s()+")"
		end
		if type==CHAR
			return "@\\"+data.chr
		end
		if type==STRING
			return "\""+data+"\""
		end
		if type==INT
			return data.to_s
		end
		if type==BOOL
			return (if data then "#t" else "#f" end)
		end
		return ""
	end
end

def ary_to_SObj(ary)
	obj=SObj.new()
	if(ary.size()==0)
		obj.set_null()
	else
		obj.set_pair(ruby_to_SObj(ary[0]) , ary_to_SObj(ary[1..ary.size-1]))
	end
	return obj
end

def ruby_to_SObj(val)
	if val.is_a? Array
		return ary_to_SObj(val)
	end
	obj=SObj.new()
	if val.is_a? Integer
		obj.set_int(val)
		return obj
	end
	if val.is_a? String
		obj.set_str(val)
		return obj
	end
	if val==true || val==false
		obj.set_bool(val)
		return obj
	end
end
