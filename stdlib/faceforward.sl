vector faceforward(vector N, I) {
    return sign(-I.Ng) * N;
}

vector faceforward(vector N, I, Nref) {
    return sign(-I.Nref) * N;
}
