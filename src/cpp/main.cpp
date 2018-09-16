#include <iostream>
#include <fstream>
#include <sstream>

#include <Eigen/Sparse>

int main() {
  Eigen::SparseMatrix<double> sm1(42840, 49770);
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

  Eigen::SparseQR<Eigen::SparseMatrix<double>, Eigen::COLAMDOrdering<int>> qr;
  qr.compute(sm1);

  std::cout << "info : " << qr.info() << std::endl;
  std::cout << "rank : " << qr.rank() << std::endl;

  return 0;
}
