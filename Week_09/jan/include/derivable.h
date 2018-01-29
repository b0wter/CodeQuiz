#pragma once

#include <string>

class Derivable {
public:
  virtual ~Derivable() {};

  virtual double eval(double) = 0;
  virtual std::string derive() = 0;
  virtual std::string print() = 0;
};
