float min(float a,b) {
    if(a < b)
	return a;
    else
	return b;
}

point min(point a,b) {
    float r0 = min(xcomp(a), xcomp(b));
    float r1 = min(ycomp(a), ycomp(b));
    float r2 = min(zcomp(a), zcomp(b));
    return point(r0, r1, r2);
}

vector min(vector a,b) {
    float r0 = min(xcomp(a), xcomp(b));
    float r1 = min(ycomp(a), ycomp(b));
    float r2 = min(zcomp(a), zcomp(b));
    return vector(r0, r1, r2);
}

normal min(normal a,b) {
    float r0 = min(xcomp(a), xcomp(b));
    float r1 = min(ycomp(a), ycomp(b));
    float r2 = min(zcomp(a), zcomp(b));
    return r = normal(r0, r1, r2);
}

color min(color a,b) {
    float r0 = min(xcomp(a), xcomp(b));
    float r1 = min(ycomp(a), ycomp(b));
    float r2 = min(zcomp(a), zcomp(b));
    return color(r0, r1, r2);
}
