module Kind
	BOOL=1
	INT=2
	STRING=3
	CHAR=4
	PAIR=5
	NULL=6
	SYMBOL=7
	SYNTAX=8
	VECTOR=9
	UNDEF=10
	PROC=11
	LAMBDA=12
end

class ScmSymbol
	attr_accessor :name
	@@syntaxes=["define","define-syntax","quote","lambda","if","set!","begin","cond","and","or","case","let","let*","letrec","do","delay","quasiquote"]
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
		return false unless @@syntaxes.index(name).nil?
		for i in 1...name.size()
			if(@@charset.index(name[i]).nil?)
				return false
			end
		end
		return true
	end
	def ==(another)
		(another.is_a? ScmSymbol) && @name==another.name
	end
	def eql?(another)
		(another.is_a? ScmSymbol) && @name==another.name
	end
	def hash()
		return 0x33c0fb18^@name.hash
	end
	def to_s
		@name
	end
	def inspect
		"["+to_s+"]"
	end
end
class ScmSyntax
	attr_accessor :name
	@@syntaxes=["define","define-syntax","quote","lambda","if","set!","begin","cond","and","or","case","let",
		"let*","letrec","do","delay","quasiquote"]
	def self.syntax()
		@@syntaxes
	end
	def initialize(name)
		raise unless ScmSyntax::valid?(name.downcase)
		@name=name.downcase
	end
	def self.valid?(name)
		!(@@syntaxes.index(name).nil?)
	end
	def ==(another)
		(another.is_a? ScmSyntax) && @name==another.name
	end
	def eql?(another)
		(another.is_a? ScmSyntax) && @name==another.name
	end
	def hash()
		return 0x33c0fb18^@name.hash
	end
	def to_s
		'syntax:'+@name
	end
	def inspect
		"["+to_s+"]"
	end
end

module RbScm
	def symbol(name)
		return ScmSymbol.new(name)
	end
	def syntax(keyword)
		return ScmSyntax.new(keyword)
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
		raise unless car.is_a? SObj
		raise unless cdr.is_a? SObj
		@type=PAIR
		@data=[car,cdr]
	end
	def set_null()
		@type=NULL
		@data=nil
	end
	def set_symbol(val)
		@type=SYMBOL
		@data=val
	end
	def set_syntax(val)
		@type=SYNTAX
		@data=val
	end
	def set_vector(val)
		@type=VECTOR
		@data=val
	end
	def set_undef()
		@type=UNDEF
		@data=nil
	end
	def set_proc(val)
		@type=PROC
		@data=val
	end
	def set_lambda(val)
		@type=LAMBDA
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
		if type==SYMBOL || type==SYNTAX
			return data.to_s
		end
		if type==VECTOR
			str='#('
			for el in data
				str+=el.to_s
				str+=' '
			end
			return (if(data.size==0) then '#(' else str[0...str.size-1] end)+')'
		end
		if type==UNDEF
			return "#<undef>"
		end
		if type==PROC
			return "<proc >"
		end
		if type==LAMBDA
			return data.to_s
		end
		return ""
	end
	def ==(another)
		return (another.is_a? SObj) &&
			@type==another.type &&
			@data==another.data
	end
	def inspect
		"?"+to_s
	end
end

module RbScm
	module_function
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
		if val.is_a? SObj
			return val
		end
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
		if val==nil
			obj.set_null()
			return obj
		end
		if val.is_a? ScmSymbol
			obj.set_symbol(val)
			return obj
		end
		if val.is_a? ScmSyntax
			obj.set_syntax(val)
			return obj
		end
		if val.is_a? Proc
			obj.set_proc(val)
			return obj
		end
		if val.is_a? LambdaClosure
			obj.set_lambda(val)
			return obj
		end
	end
	def make_str(val)
		obj=SObj.new()
		obj.set_str(val)
		return obj
	end
	def make_null()
		obj=SObj.new()
		obj.set_null
		return obj
	end
	def make_pair(car,cdr)
		obj=SObj.new()
		obj.set_pair(car,cdr)
		return obj
	end
	def make_list(ary)
		obj=SObj.new()
		if(ary.size()==0)
			obj.set_null()
		else
			obj.set_pair(ary[0], make_list(ary[1..ary.size-1]))
		end
		return obj
	end
	def make_vector(elem)
		obj=SObj.new
		obj.set_vector(elem)
		return obj
	end
	def make_symbol(name)
		obj=SObj.new
		obj.set_symbol(symbol(name))
		return obj
	end
	def make_syntax(name)
		obj=SObj.new
		obj.set_syntax(syntax(name))
		return obj
	end
	def make_undef()
		obj=SObj.new
		obj.set_undef
		return obj
	end
	def scm_true?(sobj)
		sobj.type!=BOOL || sobj.data
	end
	def scm_false?(sobj)
		sobj.type==BOOL && !sobj.data
	end
end
