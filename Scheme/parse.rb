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
	def bool?(str)
		if str[0]!='#'
			return false
		end
		return str[1]=='t' || str[1]=='f'
	end
	def parse_bool(str)
		raise 'bool(#t|#f) required in:'+str unless bool?(str)
		return RbScm::ruby_to_SObj(str[1]=='t')
	end
end # module RbScmParse

