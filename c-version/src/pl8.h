#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <string>
#include <vector>

#include "pallet.h"


typedef struct
{
  uint16_t width;
  uint16_t height;
  uint32_t offset;
  uint16_t x;
  uint16_t y;
  uint8_t extraType;
  uint8_t extraRows;
  uint16_t unk;
} __attribute__((packed)) TileHeader;

typedef struct
{
  uint16_t rleFlag;
  uint16_t numberOfTiles;
  uint32_t unk;
  TileHeader tilesHeaders;
} __attribute__((packed)) Pl8Header;

int convertPL8(std::string fileName, std::string destination, PalletColor *pallet);
