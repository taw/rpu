float mix(float x,y; float alpha) {
    return x*(1-alpha) + y*alpha;
}

point mix(point x,y; float alpha) {
    return x*(1-alpha) + y*alpha;
}

vector mix(vector x,y; float alpha) {
    return x*(1-alpha) + y*alpha;
}

normal mix(normal x,y; float alpha) {
    return x*(1-alpha) + y*alpha;
}

color mix(color x,y; float alpha) {
    return x*(1-alpha) + y*alpha;
}
