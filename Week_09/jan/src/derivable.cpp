#pragma once

#include "../include/derivable.h"

#include <sstream> 
#include <string>
#include <iomanip>

std::string Derivable::num2str(double const num) {
  char buffer[255];
  sprintf(buffer, "%g", num);
  return std::string(buffer);
}