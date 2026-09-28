#pragma once
/* SameBoy uses a real freestanding libm, not the minimal DOOM math shims. */
#define M_PI 3.14159265358979323846
double sin(double);
double cos(double);
double pow(double, double);
double sqrt(double);
double fabs(double);
double floor(double);
double ceil(double);
double round(double);
