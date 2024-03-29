#include <stdlib.h>
#include <R.h>

void sliding_window(double *depth, double *count, int *clen, int *swl, int *sws)
{
  int i,j;
  int step = 0;
  for(i=0 ; i<clen[0] ; i++)
  {
    double val = 0.0;
    for(j=0 ; j<swl[0] ; j++)
      val = val + depth[step+j];
    val = val / (double)swl[0];
    count[i] = val;
    step = step + sws[0];
  }
}