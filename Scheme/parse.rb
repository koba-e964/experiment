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
		raise 'bool(#t|#f) required in:'+str unless bool?(str)
		return [RbScm::ruby_to_SObj(str[1]=='t'),1]
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
				sobj,ind=parse_expr(ary[sum...ary.size])
				if(ary[sum+ind]!=')')
					raise 'invalid improper list:'+ary.inspectz
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
	def parse_expr(ary)
		if list?(ary)
			return parse_list(ary)
		end
		[ruby_to_SObj(nil),1]
	end
end # module RbScmParse

