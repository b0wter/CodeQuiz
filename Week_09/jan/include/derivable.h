#pragma once

#include <string>

class Derivable {
public:
  virtual ~Derivable() {};

  virtual double eval(double) const = 0;
  virtual std::string derive() const = 0;
  virtual std::string print() const = 0;

protected:
  static std::string num2str(double const num);
};