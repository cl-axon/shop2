
/* 
  sStack.h

  Simple Stack.  

  $Date: 2004/09/18 16:45:07 $
  $Revision: 1.1.1.1 $

  This is an STL deque with a stack-like interface added.  This is an
  insecure stack with all the deque interface deliberately left
  available.  

*/

#ifndef SSTACK_H
#define SSTACK_H

#include <stl.h>

template <class T>
class sStack : public deque<T>
{
public:

    // push elem onto stack
    void push(const T& elem) 
	{
	    push_front(elem);
	};

     // pop elem from stack and return it
    T pop() 
	{
	    T elem(front());
	    pop_front();
	    return elem;
	};

    // return top element, leaving it on the stack
    T& top() 
	{
	    return front();
	};
};

#endif /* SSTACK_H */
