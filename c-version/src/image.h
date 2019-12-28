#pragma once

#include <stdint.h>
#include <vector>


typedef struct
{
  uint8_t red;
  uint8_t green;
  uint8_t blue;
  uint8_t alpha;
} __attribute__((packed)) Pixel;

typedef struct
{
  std::vector<Pixel> pixels;
  uint32_t width;
  uint32_t height;
} Image;

int writePngImage(const char *filePath, Image *image);
