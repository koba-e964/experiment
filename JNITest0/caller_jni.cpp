#include "caller_jni.h"


/*
 * Class:     Caller
 * Method:    func
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_Caller_func
  (JNIEnv *, jobject, jint value)
{
	return value*value;	
}