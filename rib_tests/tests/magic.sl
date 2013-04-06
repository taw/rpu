/*Based on stanndard plastic shader */
surface
magic(
      float Ka = 1;
      float Kd =.5;
      float Ks =.5;
      float roughness =.1;
      color specularcolor = 1;)
{
    normal Nf = faceforward(normalize(N) , I);
    vector V = -normalize(I);
//    float Px = (P-E) . vector(1, 0, 0);
//    float Py = (P-E) . vector(0, 1, 0);
//    float Pz = (P-E) . vector(0, 0, 1);
    
//    float mg_r = 0.5 + clamp((Px+1) / 2, 0, 1)/2;
//    float mg_g = 0.5 + clamp((Py+1) / 2, 0, 1)/2;
//    float mg_b = 0.5 + clamp((-Pz+4) / 2, 0, 1)/2;

    float mg_r = (((N . vector(1,0,0)) + 1) / 4) + 0.5;
    float mg_g = (((N . vector(0,1,0)) + 1) / 4) + 0.5;
    float mg_b = (((N . vector(0,0,1)) + 1) / 4) + 0.5;
    
    color c = color "rgb" (mg_r, mg_g, mg_b);
    
//    c = color "rgb" (0.5, 0.5, 1.0);
    
    Oi = Os;
    Ci = Os * (c * (Ka*ambient() + Kd*diffuse(Nf))
    +    specularcolor * Ks*specular(Nf, V, roughness)) ;
}
