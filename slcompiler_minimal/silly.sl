color silly(point P; point Eye; color near_color; color far_color)
{
    vector ray = P-Eye;
    float ray_dist = sqrt(ray.ray);
    if(ray_dist < 0)
	ray_dist = 0;
    if(ray_dist > 1)
	ray_dist = 1;

    color c = near_color + (far_color-near_color) * ray_dist;

    return c;
}
