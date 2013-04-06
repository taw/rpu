float max(float a,b) {
    if(a > b)
	return a;
    else
	return b;
}

point max(point a,b) {
    float r0 = max(xcomp(a), xcomp(b));
    float r1 = max(ycomp(a), ycomp(b));
    float r2 = max(zcomp(a), zcomp(b));
    return point(r0, r1, r2);
}

vector max(vector a,b) {
    float r0 = max(xcomp(a), xcomp(b));
    float r1 = max(ycomp(a), ycomp(b));
    float r2 = max(zcomp(a), zcomp(b));
    return vector(r0, r1, r2);
}

normal max(normal a,b) {
    float r0 = max(xcomp(a), xcomp(b));
    float r1 = max(ycomp(a), ycomp(b));
    float r2 = max(zcomp(a), zcomp(b));
    return r = normal(r0, r1, r2);
}

color max(color a,b) {
    float r0 = max(xcomp(a), xcomp(b));
    float r1 = max(ycomp(a), ycomp(b));
    float r2 = max(zcomp(a), zcomp(b));
    return color(r0, r1, r2);
}
