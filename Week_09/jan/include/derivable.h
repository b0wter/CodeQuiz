#pragma once

#include <string>
#include <memory>

namespace Function {

  class Product;
  class Sum;

  class Derivable {
  public:
    virtual ~Derivable() {};

    virtual std::string print() const = 0;
    virtual std::string derive() const = 0;
    virtual std::string print_debug() const = 0;

    virtual std::ostream& operator<<(std::ostream& os);
    double operator()(double);

    Derivable& operator <<=(const Derivable& fn);
    //Sum operator+(const Derivable &fn) const;
    //Product operator*(const Derivable &fn) const;
  
    protected:
      virtual double eval(double) const = 0;
      static std::string num2str(double const num);
  };

  //typedef std::unique_ptr<Derivable> Derivable_ptr;
  typedef Derivable* Derivable_ptr;

  std::ostream& operator<<(std::ostream& out, const Derivable& fn);
}