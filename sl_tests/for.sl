float foo()
{
    float a, b=4;
    for(a=0; a<10; a=a+1)
    {
	// a,b = 0,4
	// a,b = 1,3 
	// a,b = 2,2
	// a,b = 3,2
	if (a == b)
	    continue;
	b =  b - 1;
	// a,b = 0,3
	// a,b = 1,2 
	// a,b = 3,1
	if (a > b)
	    break;
    }
    return a+b; // 4
}
