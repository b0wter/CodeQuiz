#include <iostream>
#include <fstream>

#include "driver.h"
#include "expression.h"

int main(int argc, char **argv)
{
    ExprContext context;
    derivative::MathDriver driver(context);

    bool readFile = false;

    for(int i = 1; i < argc; ++i) {
        if(argv[i] == std::string("-p")) {
            driver.traceParsing = true;
        } else if(argv[i] == std::string("-s")) {
            driver.traceScanning = true;
        } else {
            // read a file with expressions

	        std::fstream infile(argv[i]);
	        if(!infile.good()) {
		        std::cerr << "Could not open file: " << argv[i] << std::endl;
		        return 0;
	        }

            context.clearExpressions();
	        bool result = driver.parseStream(infile, argv[i]);
	        if(result) {
		        std::cout << "Expressions:" << std::endl;
		        for(unsigned int ei = 0; ei < context.expressions.size(); ++ei) {
		            std::cout << "[" << ei << "]:" << std::endl;
		            std::cout << "tree:" << std::endl;
		            context.expressions[ei]->print_expr(std::cout);
                    std::cout << std::endl;
		        }
	        }

	        readFile = true;
        }
    }

    if(readFile)
        return 0;

    std::cout << "Reading expressions from stdin" << std::endl;

    std::string line;
    while(std::cout << "$ " && std::getline(std::cin, line) && !line.empty()) {
	    context.clearExpressions();
	    bool result = driver.parseString(line, "input");

	    if (result) {
	        for (unsigned int ei = 0; ei < context.expressions.size(); ++ei) {
		    std::cout << "tree:" << std::endl;
		    context.expressions[ei]->print_expr(std::cout);
            std::cout << std::endl;
	        }
	    }
    }
}