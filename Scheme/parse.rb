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
				ary+=[cur]
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
	def num?(ary)
		#only decimal integers are admitted. TODO FIXME
		s="0123456789"
		str=ary[0]
		for ch in str.chars.map{|v|v}
			return false if s.index(ch).nil?
		end
		return true
	end
	def parse_num(ary)
		raise 'not a decimal int' unless num?(ary)
		sum=0
		str=ary[0]
		for ch in str.chars.map{|v|v}
			n=ch.ord-48
			sum=10*sum+n
		end
		return [ruby_to_SObj(sum),1]
	end
	def list?(ary)
		ary[0]=='('
	end
	def parse_list(ary)
		raise 'not list' unless list?(ary)
		sum=1
		pool=[]
		while(sum<ary.size() && ary[sum]!=')')
			if(ary[sum]=='.') #improper list
				sobj,ind=parse_expr(ary[sum+1...ary.size])
				if(ary[sum+ind+1]!=')')
					raise 'invalid improper list:'+ary.inspect
				end
				pool[pool.size-1]=make_pair(pool[pool.size-1], sobj)
				sum+=ind
				break
			end
			sobj,ind=parse_expr(ary[sum...ary.size])
			pool+=[sobj]
			sum+=ind
		end
		return [ruby_to_SObj(pool),sum+1]
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
		return [make_vector(pool),sum+1]
	end
	def parse_symbol_or_syntax(ary)
		str=ary[0]
		case
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
		when bool?(ary)
			return parse_bool(ary)
		when vector?(ary)
			return parse_vector(ary)
		end
		return parse_symbol_or_syntax(ary)
	end
end # module RbScmParse

