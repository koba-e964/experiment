/**
	This source is compiled to CLib.dll.
*/

#include "CLib.h"

extern "C"{
	EXPORT int calcHashCodeHimmel(HimmelAdler* himmel){
		int h=0xdeadc0de;
		char* ptr=himmel->name;
		for(;*ptr;++ptr){
			h*=31;
			h+=*ptr;
		}
		h+=himmel->ability;
		h+=(int)himmel->value;
		h+=(int)(himmel->value>>32);
		return h;
	}
}//extern "C"

