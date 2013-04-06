float foo(float x) {
    float a;
    float b;
    if(x>0)
    {
        if(x>1)
            a=0;
        else
            a=1;
	b=1;
    }
    else
    {
        if(x<-1)
            a=2;
        else
            a=3;
	b=2;
    }
    return a+b;
}
