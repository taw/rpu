surface
bunny_shader()
{
    normal n = N;
    if(I.N > 0.0) {
	n = -n;
    }

    // Is it normalized already ?
    vector ray_dir = L;
    ray_dir = ray_dir / length(ray_dir);

    float d = n . ray_dir;
    if (d < 0.0)
	d = 0.0;
    float oi = 0.4 + 0.6 * d;
    if(oi > 1.0)
	oi = 1.0;

    Ci = Cs;
    Oi = Os * oi;
}
