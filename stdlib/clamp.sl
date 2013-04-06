float clamp(float a, min, max) {
    if (a < min)
	return min;
    else if (a > max)
	return max;
    else
	return a;
}

point clamp(point a, min, max) {
    float r0 = clamp(xcomp(a), xcomp(min), xcomp(max));
    float r1 = clamp(ycomp(a), ycomp(min), ycomp(max));
    float r2 = clamp(zcomp(a), zcomp(min), zcomp(max));
    return point (r0, r1, r2);
}

vector clamp(vector a, min, max) {
    float r0 = clamp(xcomp(a), xcomp(min), xcomp(max));
    float r1 = clamp(ycomp(a), ycomp(min), ycomp(max));
    float r2 = clamp(zcomp(a), zcomp(min), zcomp(max));
    return vector (r0, r1, r2);
}

normal clamp(normal a, min, max) {
    float r0 = clamp(xcomp(a), xcomp(min), xcomp(max));
    float r1 = clamp(ycomp(a), ycomp(min), ycomp(max));
    float r2 = clamp(zcomp(a), zcomp(min), zcomp(max));
    return normal(r0, r1, r2);
}

color clamp(color a, min, max) {
    float r0 = clamp(xcomp(a), xcomp(min), xcomp(max));
    float r1 = clamp(ycomp(a), ycomp(min), ycomp(max));
    float r2 = clamp(zcomp(a), zcomp(min), zcomp(max));
    return color(r0, r1, r2);
}
