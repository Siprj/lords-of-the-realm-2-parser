#include <png.h>
#include <stdio.h>
#include <stdint.h>
#include <iostream>
#include <vector>

#include "image.h"


int writePngImage(const char *filePath, Image *image)
{

  // TODO: check correctnes of filePath and image
  png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
  if (png_ptr == nullptr)
  {
    // TODO: do semething about this error...
    std::cout << "error: png_create_write_struct\n";
    return -1;
  }

  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (info_ptr == nullptr)
  {
    // TODO: do semething about this error...
    std::cout << "error: png_create_info_struct\n";
    return -1;
  }

  png_set_IHDR(
    png_ptr,
    info_ptr,
    image->width,
    image->height,
    8, // bitdepth
    PNG_COLOR_TYPE_RGBA,
    PNG_INTERLACE_NONE,
    PNG_COMPRESSION_TYPE_BASE,
    PNG_FILTER_TYPE_DEFAULT
    );

  png_byte **row_pointers = reinterpret_cast<png_byte**>(
    malloc(sizeof(png_bytep) * image->height));

  if (row_pointers == nullptr)
  {
    // TODO: do semething about this error...
    std::cout << "error: row_pointers is null\n";
    return -1;
  }

  for (uint32_t i = 0; i < image->height; ++i)
  {
    row_pointers[i] = reinterpret_cast<png_byte*>(image->pixels.data() + (i * image->width));
  }

  auto file = fopen(filePath, "w");

  png_init_io(png_ptr, file);
  png_set_rows(png_ptr, info_ptr, row_pointers);

  if (setjmp(png_jmpbuf(png_ptr)))
  {
    // TODO: do semething about this error...
    std::cout<<"[write_png_file] Error during writing bytes\n";
    return -1;
  }

  png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, nullptr);

  fclose(file);
  free(row_pointers);
  png_destroy_write_struct(&png_ptr, &info_ptr);
  return 0;
}
