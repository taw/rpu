vector refract(vector I, N; float eta) {
    float IdotN = I.N;
    float k = 1 - eta*eta*(1 - IdotN*IdotN);
    return k < 0 ? (0,0,0) : eta*I - (eta*IdotN + sqrt(k))*N;
}
