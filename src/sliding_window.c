#include <stdlib.h>
#include <R.h>
#include "sliding_window.h"

void sliding_window_mean(double *data, double *out, int *start, int *n, int *len)
{
  int i,j;
  for(i=0 ; i<n[0] ; i++)
  {
    double val = 0.0;
    for(j=0 ; j<len[0] ; j++)
      val = val + data[start[i]+j];
    val = val / (double)len[0];
    out[i] = val;
  }
}

void sliding_window_sum(double *data, double *out, int *start, int *n, int *len)
{
  int i,j;
  for(i=0 ; i<n[0] ; i++)
  {
    double val = 0.0;
    for(j=0 ; j<len[0] ; j++)
      val = val + data[start[i]+j];
    out[i] = val;
  }
}

void sliding_window_min(double *data, double *out, int *start, int *n, int *len)
{
  int i,j;
  for(i=0 ; i<n[0] ; i++)
  {
    double val = data[start[i]];
    for(j=1 ; j<len[0] ; j++)
    {
      if( val > data[start[i]+j] )
        val = data[start[i]+j];
    }
    out[i] = val;
  }
}

void sliding_window_max(double *data, double *out, int *start, int *n, int *len)
{
  int i,j;
  for(i=0 ; i<n[0] ; i++)
  {
    double val = data[start[i]];
    for(j=1 ; j<len[0] ; j++)
    {
      if( val < data[start[i]+j] )
        val = data[start[i]+j];
    }
    out[i] = val;
  }
}

int double_cmp(const void *a, const void *b)
{
  double const *pa = a;
  double const *pb = b;
  double diff = *pa - *pb;
  if(diff > 0.0)
    return(1);
  if(diff < 0.0)
    return(-1);
  return(0);
}

double median(double *data, int n)
{
  // Sort the data array
  qsort(data, n, sizeof(double), double_cmp);

  // Return
  int nm = n%2;
  int i = n/2;
  if(nm == 0)
  {
    return(0.5 * (data[i]+data[i+1]));
  }
  else
  {
    return(data[i]);
  }
}

void sliding_window_median(double *data, double *out, int *start, int *n, int *len)
{
  int i,j;
  double *sub;
  sub = (double *) calloc(len[0], sizeof(double));
  for(i=0 ; i<n[0] ; i++)
  {
    for(j=0 ; j<len[0] ; j++)
      sub[j] = data[start[i]+j];
    out[i] = median(sub, len[0]);
  }
  free(sub);
}

