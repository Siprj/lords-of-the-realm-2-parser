#pragma once

#include <stdlib.h>
#include <stdint.h>

typedef struct
{
  uint8_t red;
  uint8_t green;
  uint8_t blue;
} __attribute__((packed)) PalletColor;

PalletColor* readPallet(const char *filePath);
