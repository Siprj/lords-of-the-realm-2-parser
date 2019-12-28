#include <png.h>
#include <iostream>

#include "pl8.h"
#include "pallet.h"
#include "duo.h"

const std::string assetDirectory = "/home/yrid/.steam/steam/steamapps/common/Lords of the Realm II/English/Lords of the Realm II/";
const std::string resultDirectory = "/home/yrid/pokus3/";

int main ()
{
  for (const auto &palletAndFiles : fileList)
  {
    std::string palletFile = assetDirectory + palletAndFiles.first;
    PalletColor *pallet = readPallet(palletFile.c_str());
    for (const auto &file : palletAndFiles.second)
    {
      std::string pl8FileName = assetDirectory + file;
      std::string resultFile = resultDirectory + file + ".png";

      std::cout << pl8FileName << " -> " << resultFile << "\n";
      std::cout.flush();
      convertPL8(
        pl8FileName,
        resultFile,
        pallet);
    }
    delete []pallet;
  }

  return 0;
}
