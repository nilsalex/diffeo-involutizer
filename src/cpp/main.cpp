#include <iostream>
#include <fstream>
#include <sstream>

#include <Eigen/Sparse>
#include <Eigen/SPQRSupport>

int main() {
  Eigen::SparseMatrix<double> sm1(3, 3);
  /*
  sm1.reserve(3000000);

  {
    std::vector<Eigen::Triplet<double>> tripletList;
    tripletList.reserve(3000000);
  
    std::ifstream ifile ("output.txt");
  
    std::string substr;
    int i, j;
    double v;
  
    while (ifile) {
      std::getline(ifile, substr, ',');
      if (!ifile) break;
      i = std::stoi(substr);
  
      std::getline(ifile, substr, ',');
      j = std::stoi(substr);
  
      std::getline(ifile, substr);
      v = std::stod(substr);

      tripletList.push_back(Eigen::Triplet<double>(i, j, v));
    }

    sm1.setFromTriplets(tripletList.begin(), tripletList.end());
  }
  */

  std::vector<Eigen::Triplet<double>> tripletList = {
    {0, 0, 5},
    {1, 1, 3},
  };

  sm1.setFromTriplets(tripletList.begin(), tripletList.end());

  Eigen::SPQR<Eigen::SparseMatrix<double>> spqr(sm1);

  std::cout << "info : " << spqr.info() << std::endl;
  std::cout << "rank : " << spqr.rank() << std::endl;

  return 0;
}
