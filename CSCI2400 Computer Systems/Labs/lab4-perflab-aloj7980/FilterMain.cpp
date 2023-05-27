#include <stdio.h>
#include "cs1300bmp.h"
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include "Filter.h"

using namespace std;

#include "rdtsc.h"

//
// Forward declare the functions
//
Filter * readFilter(string filename);
double applyFilter(Filter *filter, cs1300bmp *input, cs1300bmp *output);

int
main(int argc, char **argv)
{

  if ( argc < 2) {
    fprintf(stderr,"Usage: %s filter inputfile1 inputfile2 .... \n", argv[0]);
  }

  //
  // Convert to C++ strings to simplify manipulation
  //
  string filtername = argv[1];

  //
  // remove any ".filter" in the filtername
  //
  string filterOutputName = filtername;
  string::size_type loc = filterOutputName.find(".filter");
  if (loc != string::npos) {
    //
    // Remove the ".filter" name, which should occur on all the provided filters
    //
    filterOutputName = filtername.substr(0, loc);
  }

  Filter *filter = readFilter(filtername);

  double sum = 0.0;
  int samples = 0;

  for (int inNum = 2; inNum < argc; inNum++) {
    string inputFilename = argv[inNum];
    string outputFilename = "filtered-" + filterOutputName + "-" + inputFilename;
    struct cs1300bmp *input = new struct cs1300bmp;
    struct cs1300bmp *output = new struct cs1300bmp;
    int ok = cs1300bmp_readfile( (char *) inputFilename.c_str(), input);

    if ( ok ) {
      double sample = applyFilter(filter, input, output);
      sum += sample;
      samples++;
      cs1300bmp_writefile((char *) outputFilename.c_str(), output);
    }
    delete input;
    delete output;
  }
  fprintf(stdout, "Average cycles per sample is %f\n", sum / samples);

}

struct Filter *
readFilter(string filename)
{
  ifstream input(filename.c_str());

  if ( ! input.bad() ) {
    int size = 0;
    input >> size;
    Filter *filter = new Filter(size);
    int div;
    input >> div;
    filter -> setDivisor(div);
    for (int i=0; i < size; i++) {
      for (int j=0; j < size; j++) {
	int value;
	input >> value;
	filter -> set(i,j,value);
      }
    }
    return filter;
  } else {
    cerr << "Bad input in readFilter:" << filename << endl;
    exit(-1);
  }
}


double
applyFilter(class Filter *filter, cs1300bmp *input, cs1300bmp *output)
{
    long long cycStart, cycStop;
    cycStart = rdtscll();
    
    int divisor = filter->getDivisor();
    int myHeight = input->height;
    int myWidth = input->width;
    int myHeightMinus1=myHeight-1;
    int myWidthMinus1=myWidth-1;
    
    output -> width = myWidth;
    output -> height = myHeight;
    
    short filterMatrix[9] = {
        filter->get(0,0), filter->get(0,1), filter->get(0,2),
        filter->get(1,0), filter->get(1,1),	filter->get(1,2),
        filter->get(2,0), filter->get(2,1), filter->get(2,2)
    };
    
    short currentValue;

    for (int plane = 0; plane < 3; ++plane)
        
    {
        for (int row = 1; row < myHeightMinus1; ++row)
        
        {
            for (int col = 1; col < myWidthMinus1; ++col)
            {
                short* currentPixel = &input->color[plane][row-1][col-1];
                short* myFilter = &filterMatrix[0];
                currentValue = *(currentPixel++) * *(myFilter++);
                currentValue += *(currentPixel++) * *(myFilter++);
                currentValue += *(currentPixel) * *(myFilter++);
                currentPixel = &input->color[plane][row][col-1];
                currentValue += *(currentPixel++) * *(myFilter++);
                currentValue += *(currentPixel++) * *(myFilter++);
                currentValue += *(currentPixel) * *(myFilter++); 
                currentPixel = &input->color[plane][row+1][col-1];
                currentValue += *(currentPixel++) * *(myFilter++);
                currentValue += *(currentPixel++) * *(myFilter++);
                currentValue += *(currentPixel) * *(myFilter);
                currentValue /= divisor;
                currentValue = (currentValue > 255) ? 255 : currentValue;
                currentValue = (currentValue < 0) ? 0 : currentValue;
                output->color[plane][row][col] = currentValue;
        }
    }
    }
  cycStop = rdtscll();
  double diff = cycStop - cycStart;
  double diffPerPixel = diff / (output -> width * output -> height);
  fprintf(stderr, "Took %f cycles to process, or %f cycles per pixel\n",
	  diff, diff / (output -> width * output -> height));
  return diffPerPixel;
}