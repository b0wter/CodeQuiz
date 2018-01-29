#pragma once

#include <string>
#include <memory>

class Derivable {
public:
  virtual ~Derivable() {};

  virtual double eval(double) const = 0;
  virtual std::string derive() const = 0;
  virtual std::string print() const = 0;
  virtual std::string print_debug() const = 0;

protected:
  static std::string num2str(double const num);
};
//typedef std::unique_ptr<Derivable> Derivable_ptr;
typedef Derivable* Derivable_ptr;