#define EXPORT __declspec(dllexport) __stdcall

typedef struct HimmelAdler
{
	char* name;
	int ability;
	long long value;
}HimmelAdler;

extern "C"{
	EXPORT int calcHashCodeHimmel(HimmelAdler* himmel);
}//extern "C"
