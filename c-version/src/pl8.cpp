#include <algorithm>
#include <fstream>
#include <functional>
#include <iostream>
#include <stdint.h>
#include <vector>

#include "image.h"
#include "pallet.h"
#include "pl8.h"
#include "tile.h"


void fillPixel(Pixel &pixel, PalletColor &color)
{
  pixel.red = color.red * 4;
  pixel.green = color.green * 4;
  pixel.blue = color.blue * 4;
  pixel.alpha = 255;
}

void fillFromPallet(int pixelIndex, Image &image, PalletColor *pallet, uint8_t palletIndex)
{
  if (palletIndex != 0)
  {
    fillPixel(image.pixels[pixelIndex], pallet[palletIndex]);
  }
}

Tile simpleTile(TileHeader *tileHeader, std::vector<char> &fileData)
{
  return
    { tileHeader->x
    , tileHeader->y
    , tileHeader->width
    , tileHeader->height
    , 0
    , { reinterpret_cast<uint8_t*>(fileData.data() + tileHeader->offset)
      , reinterpret_cast<uint8_t*>(fileData.data() + tileHeader->offset + (tileHeader->height * tileHeader->width))
      }
    };
}

Tile isoTile(TileHeader *tileHeader, std::vector<char> &fileData)
{
  Tile tile
    { tileHeader->x
    , tileHeader->y - tileHeader->extraRows
    , tileHeader->width
    , tileHeader->height + tileHeader->extraRows
    , tileHeader->extraRows
    , std::vector<uint8_t>(tileHeader->width * (tileHeader->height + tileHeader->extraRows), 0)
    };

  int halfWidth = tileHeader->width / 2;
  int halfHeight = tileHeader->height / 2;

  int readData = tileHeader->offset;

  uint8_t *data = reinterpret_cast<uint8_t*>(fileData.data());

  // Fill top half
  for (int y = 0; y < halfHeight; ++y)
  {
    int rowStart = (halfHeight - 1 - y) * 2;
    int rowStop = rowStart + (y * 4) + 2;

    for (int x = rowStart; x < rowStop; ++x)
    {
      uint8_t palletIndex = data[readData];
      ++readData;

      tile.pixels[((y + tileHeader->extraRows) * tileHeader->width) + x] = palletIndex;
    }
  }
  // Fill bottom half
  for (int y = halfHeight; y < tileHeader->height; ++y)
  {
    int rowStart = (halfHeight - 1 - (tileHeader->height - y - 1)) * 2;
    int rowStop = rowStart + ((tileHeader->height - y -1) * 4) + 2;

    for (int x = rowStart; x < rowStop; ++x)
    {
      uint8_t palletIndex = data[readData];
      ++readData;

      tile.pixels[((y + tileHeader->extraRows) * tileHeader->width) + x] = palletIndex;
    }
  }

  // Fill extra rows
  for (int y_ = tileHeader->extraRows; y_ > 0; --y_)
  {
    int rightOffset = tileHeader->extraType == 3
      ? halfWidth + 1 : tileHeader->width;
    int leftOffset = tileHeader->extraType == 4
      ? halfWidth - 1
      : 0;

    for (int x = leftOffset; x < rightOffset; ++x)
    {
      int y = x <= halfWidth
        ?  y_ + (halfHeight - 1) - (x / 2)
        : y_ + (x / 2) - (halfHeight - 1);

      uint8_t palletIndex = data[readData];
      ++readData;

      tile.pixels[(y * tile.width) + x] = palletIndex;
    }
  }
  return tile;
}

Tile rleTile(TileHeader *tileHeader, std::vector<char> &fileData)
{
  Tile tile
    { tileHeader->x
    , tileHeader->y
    , tileHeader->width
    , tileHeader->height
    , 0
    , std::vector<uint8_t>(tileHeader->width * tileHeader->height)
    };

  uint8_t* data = reinterpret_cast<uint8_t*>(fileData.data());

  int readData = tileHeader->offset;
  int filled = 0;
  int toFill = tileHeader->width * tileHeader->height;
  while (filled < toFill && readData < fileData.size())
  {
    int numberOfOpaquePixels = data[readData];
    ++readData;

    if (numberOfOpaquePixels == 0)
    {
      int numberOfTransparetnPixels = data[readData];
      ++readData;

      for (int i = 0; i < numberOfTransparetnPixels && filled < toFill; ++i)
      {
        tile.pixels[filled] = 0;
        ++filled;
      }
    }
    else
    {
      for (int i = 0; i < numberOfOpaquePixels && filled < toFill; ++i)
      {
        uint8_t palletIndex = data[readData];
        ++readData;

        tile.pixels[filled] = palletIndex;
        ++filled;
      }
    }
  }
  return tile;
}

void emplaceTile(Image &image, Tile &tile, PalletColor *pallet)
{
  int i = 0;
  for (int y = tile.y; y < tile.y + tile.height; ++y)
  {
    for (int x = tile.x; x < tile.width + tile.x; ++x)
    {
      fillFromPallet((y * image.width) + x, image, pallet, tile.pixels[i]);
      ++i;
    }
  }
}

bool fWidth(Tile h1, Tile h2)
{
  return h1.width + h1.x < h2.width + h2.x;
}

bool fHeight(Tile h1, Tile h2)
{
  return h1.height + h1.y < h2.height + h2.y;
}

int convertPL8(std::string fileName, std::string destination, PalletColor *pallet)
{
  std::ifstream file(fileName, std::ios_base::binary);

  file.seekg (0, file.end);
  auto fileSize = file.tellg();
  file.seekg (0, file.beg);

  std::vector<char> fileData;
  fileData.resize(fileSize);
  file.read(fileData.data(), fileSize);

  auto fileDataPtr =  fileData.data();
  Pl8Header *pl8Header = reinterpret_cast<Pl8Header*>(fileDataPtr);

  TileHeader *tileHeaders = &pl8Header->tilesHeaders;
  std::vector<Tile> tileSet;

  for (int i = 0; i < pl8Header->numberOfTiles; ++i)
  {
    TileHeader *currentTileHeader = &tileHeaders[i];
    if (currentTileHeader->height == 0 || currentTileHeader->width == 0)
    {
      continue;
    }

    if (pl8Header->rleFlag & 0x0001) // RLE encoding
    {
      tileSet.emplace_back(rleTile(currentTileHeader, fileData));
    }
    else if(currentTileHeader->extraType == 0)
    {
      tileSet.emplace_back(simpleTile(currentTileHeader, fileData));
    }
    else
    {
      tileSet.emplace_back(isoTile(currentTileHeader, fileData));
    }
  }

  auto rightmost =
    std::max_element(tileSet.begin(), tileSet.end(), fWidth);
  uint32_t width = rightmost->x + rightmost->width;

  auto bottommost =
    std::max_element(tileSet.begin(), tileSet.end(), fHeight);
  uint32_t height = bottommost->y + bottommost->height;

  Image image;
  image.width = width;
  image.height = height;
  image.pixels.resize((height + 120) * width, {0,0,0,0});

  for (auto &tile : tileSet)
  {
    emplaceTile(image, tile, pallet);
  }
  writePngImage(destination.c_str(), &image);
//  for (auto i = 0; i < tileSet.size(); ++i)
//  {
//    std::cout << "writing tile: " << i << "\n";
//    Tile tile = tileSet[i];
//    Image image;
//    image.height = tile.height;
//    image.width = tile.width;
//    image.pixels.resize(tile.width * tile.height);
//    tile.x = 0;
//    tile.y = 0;
//    emplaceTile(image, tile, pallet);
//
//    std::string destination2 = destination + std::to_string(i) + ".png";
//    writePngImage(destination2.c_str(), &image);
//  }

  return 0;
}
