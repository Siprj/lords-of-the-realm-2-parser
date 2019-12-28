#include "pallet.h"

#include <iostream>
#include <fstream>


PalletColor* readPallet(const char *filePath)
{
  std::ifstream file(filePath, std::ios_base::binary);

  file.seekg (0, file.end);
  auto fileSize = file.tellg();
  file.seekg (0, file.beg);

  if (fileSize != 3 * 256)
  {
    std::cerr << "ERROR: Pallet file has incorrect size!\n";
    return nullptr;
  }

  PalletColor *pallet = new PalletColor[256];

  file.read(reinterpret_cast<char *>(pallet), 256 * 3);

  return pallet;
}
