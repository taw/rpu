#define change_vector(P1,P2)    \
        if(normalize(P1).normalize(P2)>0) {     \
                P1=-P1; \
        }       \
        P1=normalize(P1);       \



#define judge_foreback(P1,P2,P3)        \
        judge=0;        \
        dummyP4=P2-P1;  \
        if (length(dummyP4)>ZERO) {     \
                judge=dummyP4.P3;       \
        }       \



#define get_intersectpoint_Rt04(P00,P01,P02,P03,P1,P2);	\
	dummyP1=P01-P00;	\
	dummyP2=P02-P00;	\
	Rt_N=dummyP2^dummyP1;	\
	Rt_N=normalize(Rt_N);	\
	change_vector(Rt_N,P2);	\
	aa=xcomp(Rt_N);	\
	bb=ycomp(Rt_N);	\
	cc=zcomp(Rt_N);	\
	dd=-aa*xcomp(P00)-bb*ycomp(P00)-cc*zcomp(P00);	\
	\
	judge=aa*xcomp(P2)+bb*ycomp(P2)+cc*zcomp(P2);	\
	if (judge!=0) {	\
		tt=(-aa*xcomp(P1)-bb*ycomp(P1)-cc*zcomp(P1)-dd)/judge;	\
		intersectP=point(tt*xcomp(P2)+xcomp(P1),tt*ycomp(P2)+ycomp(P1),tt*zcomp(P2)+zcomp(P1));	\
	}	\
			
                                                                    





#define transform_to_cameraspace(P0,tx,ty,tz,rx,ry,rz)     \
        P0_x=xcomp(P0);    \
        P0_y=ycomp(P0);    \
        P0_z=zcomp(P0);    \
        \
        P0_x_dummy=P0_x;      \
        P0_y_dummy=cos(radians(rx))*P0_y-sin(radians(rx))*P0_z;      \
        P0_z_dummy=sin(radians(rx))*P0_y+cos(radians(rx))*P0_z;      \
        P0_x=P0_x_dummy;      \
        P0_y=P0_y_dummy;      \
        P0_z=P0_z_dummy;      \
        \
        P0_y_dummy=P0_y;      \
        P0_x_dummy=cos(radians(ry))*P0_x-sin(radians(ry))*P0_z;      \
        P0_z_dummy=sin(radians(ry))*P0_x+cos(radians(ry))*P0_z;      \
        P0_x=P0_x_dummy;      \
        P0_y=P0_y_dummy;      \
        P0_z=P0_z_dummy;      \
        \
        P0_z_dummy=P0_z;      \
        P0_x_dummy=cos(radians(rz))*P0_x-sin(radians(rz))*P0_y;      \
        P0_y_dummy=sin(radians(rz))*P0_x+cos(radians(rz))*P0_y;      \
        P0_x=P0_x_dummy;      \
        P0_y=P0_y_dummy;      \
        P0_z=P0_z_dummy;      \
        \
        P0_x=P0_x+tx;    \
        P0_y=P0_y+ty;    \
        P0_z=P0_z+tz;    \
	\
        P0=point(P0_x,P0_y,P0_z);   \



#define judge_intersect_2line(lP1,lP2,P1,dl_md,dl_shrp,judge,r_u,eye_to);	\
	lP1_screen=(r_u/zcomp(lP1))*lP1;	\
	lP2_screen=(r_u/zcomp(lP2))*lP2;	\
	\
	if (xcomp(lP1_screen)!=xcomp(lP2_screen)) {   \
		A=(ycomp(lP2_screen)-ycomp(lP1_screen))/(xcomp(lP2_screen)-xcomp(lP1_screen));      \
		B=-1;   \
		C=(xcomp(lP2_screen)*ycomp(lP1_screen)-xcomp(lP1_screen)*ycomp(lP2_screen))/(xcomp(lP2_screen)-xcomp(lP1_screen));        \
		float tmp1 = A*xcomp(eye_to); \
		float tmp2 = B*ycomp(eye_to); \
		float tmp3 = sqrt(pow(A,2)+pow(B,2)); \
		dist=abs(tmp1+tmp2+C)/tmp3;    \
		float dist2=clamp((-dist+dl_md)/dl_md,0,1);    \
		judge=pow(dist2,dl_shrp); \
	}       \
	else {  \
		if (ycomp(lP1_screen)!=ycomp(lP2_screen)) {   \
			dist=abs(xcomp(eye_to)-xcomp(lP1_screen));     \
			dist=clamp((-dist+dl_md)/dl_md,0,1);    \
			judge=pow(dist,dl_shrp);        \
		}       \
		else {	\
			judge=0;	\
		}	\
	}       \



#define calc_line(a,b,c,d,e,f,g,h,i,j,k,l,ls,lzs,lt,lzt)        \
        floor_s=floor(a);       \
        floor_t=floor(b);       \
        mod_s=a-floor_s;        \
        mod_t=b-floor_t;        \
        \
        line_center_s=noise(c*e+floor_t*g);     \
        line_center_t=noise(d*f+floor_s*h);     \
        \
        ls=(mod_t-line_center_s)/(i/2); \
        ls=clamp(ls,-1,1);      \
        theta_s=acos(ls);       \
        lzs=sin(theta_s)*k;     \
        lzs=smoothstep(0,1,lzs);        \
        \
/*      ls*=i/2;        \
        lzs*=i/2;*/     \
        \
        lt=(mod_s-line_center_t)/(j/2); \
        lt=clamp(lt,-1,1);      \
        theta_t=acos(lt);       \
        lzt=sin(theta_t)*l;     \
        lzt=smoothstep(0,1,lzt);        \
        \
/*      lt*=j/2;        \
        lzt*=j/2;*/     \

#define quantize(a,b,c) \
        a*=b;   \
/*        a=color(clamp(floor(comp(a,0)+c*b*(2*random()-1))/b,0,1),clamp(floor(comp(a,1)+c*b*(2*random()-1))/b,0,1),clamp(floor(comp(a,2)+c*b*(2*random()-1))/b,0,1));*/    \
        rand=2*random()-1;   \
        a=color(clamp(floor(comp(a,0)+c*rand)/b,0,1),clamp(floor(comp(a,1)+c*rand)/b,0,1),clamp(floor(comp(a,2)+c*rand)/b,0,1));    \


imager
KTcanvasI
(
float ZERO=0.001,noise_max=0.75;
float bit=256,ditheramplitude=0.05;
float Kd=1;

/*float screenw_x=5,screenw_y=5;
string projection_type="ortho";*/
float screenw_x=3,screenw_y=2;
string projection_type="pers";
float fov=60;

/*float x_res=240,y_res=240;*/
/*float x_res=380,y_res=380;*/
/*float x_res=600,y_res=400;*/
float x_res=768,y_res=512;
float s_shadingrate=0.25,t_shadingrate=0.25;
float s_samplingrate=3,t_samplingrate=3;
/*float s_shadingrate=1,t_shadingrate=1;
float s_samplingrate=1,t_samplingrate=1;*/

float s_line_number=80,t_line_number=80;
float s_line_width=0.75,t_line_width=0.75;
float s_line_amp=1,t_line_amp=1;
float s_line_disp=1,t_line_disp=1;
float s_width_freq=0,t_width_freq=0;
float s_line_movefreq=102.39,t_line_movefreq=-45.77;

float s_line_number_small=160,t_line_number_small=160;
float s_line_width_small=1,t_line_width_small=1;
float s_line_amp_small=1,t_line_amp_small=1;
float s_width_freq_small=12.3,t_width_freq_small=50.54;
float s_line_movefreq_small=-12.39,t_line_movefreq_small=105.77;

float s_line_dispfreq=5,t_line_dispfreq=5;
float s_line_freq=8,t_line_freq=8;

string big_line="on",small_line="on";

float ink_sharpness=0.4,ink_blur=1,cs_intfreq=50,ink_brightness=1;
string ink_type="draw";
vector from_draw_dir=vector(1,1,0.5),to_draw_dir=vector(0,0,0);
float draw_amp_min=0.5,draw_amp_max=1;


float octaves=8,lambda=2.17,omega=0.7;
float texturescale=2,offset=0.0;
vector from=vector (1,1,2),to=vector(0,0,0);

float ka=0,kd=1;
color cs_canvas1=color(0.95,0.9,0.75),cs_canvas2=color(0.8,0.76,0.6),cs_dirtycanvas=color(0.4,0.38,0.3);
color cs_dline=color(0,0,0);


point Rt00[6]={(-1,-1,-1),(-1,-1,1),(-1,1,1),(1,1,1),(-1,-1,1),(-1,1,1)};
point Rt01[6]={(1,-1,-1),(1,-1,1),(-1,-1,1),(1,-1,1),(1,-1,1),(1,1,1)};
point Rt02[6]={(-1,1,-1),(-1,1,1),(-1,1,-1),(1,1,-1),(-1,-1,-1),(-1,1,-1)};
point Rt03[6]={(1,1,-1),(1,1,1),(-1,-1,-1),(1,-1,-1),(1,-1,-1),(1,1,-1)};
float rotate_matrix[3]={15,30,0};
float translate_matrix[3]={0,0,7.5};

string drawline="on";
float dline_number_max=6;
float dline_max_dist=0.0075,dline_sharpness=0.1,dline_draw_length=1.2;

float c_h_pow=1;
string quantization="on";
string dessin="on";
string do_imager="on";
)

{
float samples_s,samples_t;
float firstsample_s,firstsample_t;
float ss,tt;
float ss_small,tt_small;
float floor_s,floor_t,mod_s,mod_t;
float line_center_s,line_center_t;
float line_width_s,line_width_t;
float line_width_s_small,line_width_t_small;
float line_s,line_t,line;
float line_s_small,line_t_small;
float theta_s,theta_t;
float zs,zt;
float zs2,zt2;
float zs_small,zt_small;

float ink_int,sample_ink_int;
color sample_ink_cs,c_hsv;
float c_h;

uniform float i,j,l;
float k,freq;
point PP,PP_s,PP_t;
vector sN,tN,NN,first_NN,lightV,draw_dir;
float sample_d,d;
float value,value_sample;
float value_s,value_t,value_sample_s,value_sample_t;

color canvas_cs,ink_cs,ci2;
color s_cs,t_cs,s_cs_small,t_cs_small;

float max,max1,max2;
float first_max;

point lP1,lP2;
point lP1_screen,lP2_screen;
float x_trans,y_trans,z_trans,x_rotate,y_rotate,z_rotate;
float dline_int,sample_dline_int;

float judge;

float P0_x,P0_y,P0_z;
float P0_x_dummy,P0_y_dummy,P0_z_dummy;
float lP1_x,lP1_y,lP1_z,lP2_x,lP2_y,lP2_z;
float lP1_x_dummy,lP1_y_dummy,lP1_z_dummy,lP2_x_dummy,lP2_y_dummy,lP2_z_dummy;
float A,B,C;
float dist,dl_md,dl_shrp;

point Eye;
vector Eye_to,eye_to;
point Rt00C,Rt01C,Rt02C,Rt03C;
point trace_PO,trace_N;
float trace_Rt_index;
point trace_Rt[5];
uniform float count;

float intersect,judgeintersect,hit;
float inout;
point intersectP,dummy_intersectP;
point P00,P01,P02,P03;
point P0,P1,P2,P3,P4,P5,P6;
point dummyP,dummyP1,dummyP2,dummyP3,dummyP4,Rt_N;
point judgeP1,judgeP2;
point centerP_2line;


float trace_d,trace_d_min,trace_d_max;

float rotate_x,rotate_y,rotate_z,j_rotate;
float trace_judge,fore_judge;
float transformPx,transformPy,transformPz;
float box_x_min,box_x_max,box_y_min,box_y_max;

float a,b,e,f,g,h;
float aa,bb,cc,dd;
float rand;
vector spaceV,Pyz;

float r_unit,r_bound,r_u;


float printt;



if (do_imager=="on") {

d=0;
ink_int=0;
ink_cs=0;
value=0;
value_s=0;
value_t=0;

for (i=0;i<=s_samplingrate;i=i+1) {

samples_s=(s+i*s_shadingrate)/x_res;
if (i==0) {
 firstsample_s=samples_s;
}
ss=samples_s*s_line_number;
line_width_s=s_line_width*clamp((noise(samples_s*s_width_freq)/noise_max),0,1);

ss_small=samples_s*s_line_number_small;
line_width_s_small=s_line_width_small*clamp((noise(samples_s*s_width_freq_small)/noise_max),0,1);

for (j=0;j<=t_samplingrate;j=j+1) {

samples_t=(t+j*t_shadingrate)/y_res;
samples_t=1-samples_t;
if (j==0) {
 firstsample_t=samples_t;
}
tt=samples_t*t_line_number;
line_width_t=t_line_width*clamp((noise(samples_t*t_width_freq)/noise_max),0,1);

tt_small=samples_t*t_line_number_small;
line_width_t_small=t_line_width_small*clamp((noise(samples_t*t_width_freq_small)/noise_max),0,1);


line_s=0;
line_t=0;
line_s_small=0;
line_t_small=0;
zs=0;
zt=0;
zs_small=0;
zt_small=0;

if (big_line=="on") {
        calc_line(ss,tt,samples_s,samples_t,s_line_freq,t_line_freq,s_line_movefreq,t_line_movefreq,
        line_width_s,line_width_t,s_line_amp,t_line_amp,line_s,zs,line_t,zt);
}

if (small_line=="on") {
        calc_line(ss_small,tt_small,samples_s,samples_t,s_line_freq,t_line_freq,s_line_movefreq_small,t_line_movefreq_small,
        line_width_s_small,line_width_t_small,s_line_amp_small,t_line_amp_small,line_s_small,zs_small,line_t_small,zt_small);
}

ci2=Ci;
if (quantization=="on") {
	ci2=quantize(ci2,bit,ditheramplitude);
}
ci2*=ink_brightness;

s_cs=mix(color(1,1,1),ci2,pow(zs,ink_blur));
t_cs=mix(color(1,1,1),ci2,pow(zt,ink_blur));
s_cs_small=mix(color(1,1,1),ci2,pow(zs_small,ink_blur));
t_cs_small=mix(color(1,1,1),ci2,pow(zt_small,ink_blur));

/*ink_cs+=s_cs*t_cs*s_cs_small*t_cs_small;*/
sample_ink_cs=s_cs*t_cs*s_cs_small*t_cs_small;
if (dessin=="on") {
	sample_ink_cs=ctransform("hsv",sample_ink_cs);
	c_h=comp(sample_ink_cs,2);
	sample_ink_cs=color(c_h,c_h,c_h);
} 
ink_cs+=sample_ink_cs;


if ((zs<ZERO) && (zt<ZERO) && (zs_small<ZERO) && (zt_small<ZERO)) {
 max=0;
 NN=vector(0,0,1);
}
else {
 max1=max(zs,zt);
 max2=max(zs_small,zt_small);
 max=max(max1,max2);

if (max==zs) {
 NN=vector(0,line_s,zs);
}
if (max==zt) {
 NN=vector(line_t,0,zt);
}
if (max==zs_small) {
 NN=vector(0,line_s_small,zs_small);
}
if (max==zt_small) {
 NN=vector(line_t_small,0,zt_small);
}

}


NN=normalize(NN);
lightV=normalize(from-to);
sample_d=NN.lightV;
/*sample_d*=Kd;*/
sample_d+=1;
sample_d/=2;
d+=sample_d;

draw_dir=normalize(from_draw_dir-to_draw_dir);
sample_ink_int=NN.draw_dir;
sample_ink_int=clamp(sample_ink_int,0,1)*smoothstep(draw_amp_min,draw_amp_max,max);
/*sample_ink_int=smoothstep(0.85,1,max);*/
if (dessin=="on") {
	c_hsv=ctransform("hsv",sample_ink_cs);
	c_h=comp(c_hsv,2);
	c_h=pow(c_h,c_h_pow);
	sample_ink_int*=(1-c_h);
}
ink_int+=sample_ink_int;


}

}

d=d/(1+s_samplingrate)*(1+t_samplingrate);
ink_int=ink_int/(1+s_samplingrate)*(1+t_samplingrate);
ink_cs=ink_cs/(1+s_samplingrate)*(1+t_samplingrate);
/*ink_cs*=ink_brightness;*/
ink_int=pow(ink_int,ink_sharpness);


dline_int=0;
if (drawline=="on") {

trace_d_min=10000000;
trace_d_max=-1000000;
dline_int=0;
 x_rotate=rotate_matrix[0];
 y_rotate=rotate_matrix[1];
 z_rotate=rotate_matrix[2];
 x_trans=translate_matrix[0];
 y_trans=translate_matrix[1];
 z_trans=translate_matrix[2];

if (projection_type=="ortho") {
 Eye=point((firstsample_s*screenw_x)-(screenw_x/2),(firstsample_t*screenw_y)-(screenw_y/2),0);
 Eye_to=vector(0,0,1);
}
else
{
 r_unit=tan(radians(90-(fov/2)));
 Eye=point(0,0,0);
 Eye_to=vector((firstsample_s*screenw_x)-(screenw_x)/2,(firstsample_t*screenw_y)-(screenw_y)/2,r_unit);
}

dline_int=0;
l=0;

while ( (dline_int==0)&&(l<6))
{
	Rt00C=Rt00[l];
	Rt01C=Rt01[l];
	Rt02C=Rt02[l];
	Rt03C=Rt03[l];

	transform_to_cameraspace(Rt00C,x_trans,y_trans,z_trans,x_rotate,y_rotate,z_rotate);
	transform_to_cameraspace(Rt01C,x_trans,y_trans,z_trans,x_rotate,y_rotate,z_rotate);
	transform_to_cameraspace(Rt02C,x_trans,y_trans,z_trans,x_rotate,y_rotate,z_rotate);
	transform_to_cameraspace(Rt03C,x_trans,y_trans,z_trans,x_rotate,y_rotate,z_rotate);

	get_intersectpoint_Rt04( Rt00C,Rt01C,Rt02C, Rt03C,Eye, Eye_to );

	judge_foreback( Eye, intersectP,Eye_to);

	if( judge>0 )
	{
		trace_Rt[0]=Rt00C;
		trace_Rt[1]=Rt02C;
		trace_Rt[2]=Rt03C;
		trace_Rt[3]=Rt01C;
		trace_Rt[4]=Rt00C;

		count=0;
		while ((dline_int==0)&&(count<4))
		{
			centerP_2line=(trace_Rt[count]+trace_Rt[count+1])/2;
			r_bound=(distance(trace_Rt[count],trace_Rt[count+1])/2)*dline_draw_length;
			if (distance(intersectP,centerP_2line)<=r_bound)
			{
				judge_intersect_2line( trace_Rt[count],
						               trace_Rt[count+1],
						               intersectP,
				                       dline_max_dist,
				                       dline_sharpness,
				                       dline_int,
				                       r_unit,
				                       Eye_to );
			}
			count+=1;
		}
	}
	l+=1;
}

}

canvas_cs=mix(cs_canvas1,cs_canvas2,random());
Ci=d*mix(mix(canvas_cs,ink_cs,ink_int*alpha),cs_dline,ink_int*dline_int);
}

}
