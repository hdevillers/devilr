#include <stdlib.h>

void sliding_window_mean(double *data, double *out, int *start, int *n, int *len);
void sliding_window_sum(double *data, double *out, int *start, int *n, int *len);
void sliding_window_min(double *data, double *out, int *start, int *n, int *len);
void sliding_window_max(double *data, double *out, int *start, int *n, int *len);
void sliding_window_median(double *data, double *out, int *start, int *n, int *len);
int double_cmp(const void *f, const void *s);
double median(double *data, int n);

