#pragma once

#include "../include/derivable.h"

#include <sstream> 
#include <string>
#include <iomanip>

#include <iostream>
#include <stdio.h>

namespace Function {

std::string Derivable::num2str(double const num) {
  char buffer[255];
  sprintf(buffer, "%g", num);
  return std::string(buffer);
}

double Derivable::operator()(double num) {
  std::cout << "eval " << print_debug() << " with "  << eval(num) << std::endl;
  return eval(num);
}

std::ostream& Derivable::operator<<(std::ostream& out)
{
   return out << print();
}

Derivable& Derivable::operator <<=(const Derivable& fn) {

}
/*
Sum Derivable::operator+(const Derivable &fn) const {
  Sum s;
  return s;
}

Product Derivable::operator*(const Derivable &fn) const {
  Product p;
  return p;
}
*/
} /* namespace  Function */