surface simple( float Ks = .7, Kd = .6, Ka = .1, roughness = .04 )
{
	normal Nn = normalize(N);
    normal Nf = faceforward(Nn, I);
    vector V = normalize(-I);

	uniform string raytype = "unknown";
	
	rayinfo( "type", raytype );

	if( raytype == "subsurface" )
	{
    	Ci = Ka*ambient() + Kd*diffuse(Nn);
	}
	else
	{
		Ci = subsurface(P) + Ks * specular(Nf, V, roughness);		
	}
}
