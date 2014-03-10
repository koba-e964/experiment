require './main.rb'
module RbScmTokenize
	module_function
	@@delim='()";'
	def tokenize(str) # String->Array(String)
		ary=[]
		ptr=0
		cur=''
		while ptr<str.size()
			ch=str[ptr]
			case ch
			when '('
				ary+=[cur]
				ptr+=1
				ary+=['(']
				cur=''
				next
			when ')'
				ary+=[cur]
				ptr+=1
				ary+=[')']
				cur=''
				next
			when ';'
				ary+=[cur]
				while ptr<str.size() && (str[ptr]!="\r" || str[ptr]!="\n")
					ptr+=1
				end
				if ptr>=str.size()
					break
				end
				ptr+=1
				if(str[ptr+1]=="\r" ||str[ptr+1]=="\n")
					ptr+=1
				end
				next
			when '"'
				ptr+=1
				ary+=[cur]
				cur=''
				closed=false
				while ptr<str.size() &&str[ptr]!='"'
					if str[ptr]=='\\'
						ptr+=1
						if(str[ptr]=='\\'||str[ptr]=='"')
							cur+=str[ptr]
						else
							raise 'invalid escaped sequence:\\'+str[ptr]
						end
					else
						cur+=str[ptr]
					end
					ptr+=1
				end
				closed=str[ptr]=='"'
				if(!closed)
					raise 'string literal not closed'
				end
				ary+=['"'+cur] # ad-hoc fix. a symbol with first char '"' is regarded as a string.
				ptr+=1
				cur=''
				next
			when '#'
				if str[ptr+1]=='('
					ary+=['#(']
					ptr+=2
					cur=''
					next
				end
				cur+=ch
				ptr+=1
				next
			when "'"
				ary+=[cur,"'"]
				ptr+=1
				cur=''
				next
			when "`"
				ary+=[cur,"`"]
				ptr+=1
				cur=''
				next
			when ","
				ary+=[cur]
				if(str[ptr+1]=='@')
					ary+=[",@"]
					ptr+=2
					cur=''
					next
				end
				ptr+=1
				cur=''
				next
			end
			if ch.ord<=32 #space
				if cur=='#\\' # character literal
					cur+=ch # space
				end
				ary+=[cur]
				cur=''
				ptr+=1
				next
			end
			cur+=ch
			ptr+=1
		end
		ary+=[cur]
		ary.delete('')
		return ary
	end
end #module RbScmTokenize

module RbScmParse
	extend RbScm
	include RbScm
	@@syntaxes=["quote","lambda","if","set!","begin","cond","and","or","case","let",
		"let*","letrec","do","delay","quasiquote"]
	def bool?(ary)
		return ary[0]=='#t' || ary[0]=='#f'
	end
	def parse_bool(ary)
		str=ary[0]
		raise 'bool(#t|#f) required in:'+str unless bool?(ary)
		return [RbScm::ruby_to_SObj(str[1]=='t'),1]
	end
	#checks if ary[0] is an integral literal. This method doesn't support floating-point numbers.
	def num?(ary)
		s="0123456789abcdef"
		radix=10 #default
		str=ary[0]
		if(str[0]=='#') #radix prefix
			radix={'b'=>2,'o'=>8,'d'=>10,'x'=>16}[str[1]]
			return false if radix.nil?
			str=str[2...str.size]
		end
		if(str[0]=='-'||str[0]=='+') #signum
			str=str[1...str.size] #single '+'/'-' was allowed
		end
		str.size!=0 or return false #literal has at lease one digit
		s=s[0...radix] #digits must be less than radix
		for ch in str.chars.map{|v|v}
			ch.downcase!
			return false if s.index(ch).nil?
		end
		return true
	end
	def parse_num(ary) #ary is an array
		raise 'not an int' unless num?(ary)
		s="0123456789abcdef"
		sum=0
		radix=10 #default
		str=ary[0]
		sgn=1
		if(str[0]=='#') #radix prefix
			radix={'b'=>2,'o'=>8,'d'=>10,'x'=>16}[str[1]]
			radix.nil? and raise 'invalid numeric:'+str
			str=str[2...str.size]
		end
		if(str[0]=='-'||str[0]=='+') #signum
			if(str[0]=='-')
				sgn=-1
			end
			str=str[1...str.size] #single '+'/'-' was allowed
		end
		s=s[0...radix] #digits must be less than radix
		for ch in str.chars.map{|v|v}
			ch.downcase!
			n=s.index(ch)
			sum=radix*sum+n
		end
		return [ruby_to_SObj(sum*sgn),1]
	end
	def char?(ary)
		str=ary[0]
		return str[0..1]=='#\\'
	end
	def parse_char(ary)
		char?(ary) or raise 'not char'
		str=ary[0]
		c=str[2...str.size()]
		if c.size()==1 # a letter
			return [make_char(c),1]
		end
		# c is a character name or hexadecimal
		if c[0]=='x' #hexadecimal
			i=c[1..c.size()].to_i(16)
			if i<0 or i>=128 # out of ascii code
				raise 'out of range:'+str
			end
			return [make_char(i.chr),1]
		end
		# c is a character name (space/newline)
		t=nil
		case c
		when 'space'
			t=' '
		when 'newline'
			t=10.chr
		else
			raise 'invalid character name:'+str
		end
		return [make_char(t),1]
	end
	def list?(ary)
		ary[0]=='('
	end
	def parse_list(ary)
		raise 'not list' unless list?(ary)
		sum=1
		pool=[]
		fin=false
		acc=nil #returned list
		while(sum<ary.size())
			if(ary[sum]=='.') #improper list
				sobj,ind=parse_expr(ary[sum+1...ary.size])
				if(ary[sum+ind+1]!=')')
					raise 'invalid improper list:'+ary.inspect
				end
				acc=sobj
				sum+=ind+2
				fin=true
				break
			end
			if(ary[sum]==')')
				fin=true
				sum+=1
				acc=make_null
				break
			end
			sobj,ind=parse_expr(ary[sum...ary.size])
			pool+=[sobj]
			sum+=ind
		end
		fin or raise 'invalid list, ")" not found:'+ary.inspect
		i=pool.size-1
		while i>=0
			acc=make_pair(pool[i],acc)
			i-=1
		end
		return [acc,sum]
	end
	def vector?(ary)
		return ary[0]=='#('
	end
	def parse_vector(ary)
		raise 'not vector' unless vector?(ary)
		sum=1
		pool=[]
		while(sum<ary.size() && ary[sum]!=')')
			sobj,ind=parse_expr(ary[sum...ary.size])
			pool+=[sobj]
			sum+=ind
		end
		return [make_const_vector(pool),sum+1]
	end
	def quote?(ary) # abbreviated quote "'"
		return ary[0]=="'"
	end
	def parse_quote(ary)
		quote?(ary) or raise 'not quote:'+ary.inspect
		expr,len=parse_expr(ary[1...ary.size()])
		return [ruby_to_SObj([syntax('quote'),expr]),1+len]
	end
	def parse_symbol_or_syntax_or_string(ary)
		str=ary[0]
		case
		when str[0]=='"' #string
			return [make_str(str[1...str.size()]),1]
		when ScmSymbol::valid?(str)
			return [make_symbol(str),1]
		when ScmSyntax::valid?(str)
			return [make_syntax(str),1]
		end
		raise 'neither symbol nor syntax:'+str
	end
	def parse_expr(ary)
		case
		when list?(ary)
			return parse_list(ary)
		when num?(ary)
			return parse_num(ary)
		when char?(ary)
			return parse_char(ary)
		when bool?(ary)
			return parse_bool(ary)
		when vector?(ary)
			return parse_vector(ary)
		when quote?(ary)
			return parse_quote(ary)
		end
		return parse_symbol_or_syntax_or_string(ary)
	end
end # module RbScmParse

