#include <stdint.h>
#include <vector>

struct Tile
{
  int x;
  int y;
  int width;
  int height;
  int extraRows;
  std::vector<uint8_t> pixels;
};
