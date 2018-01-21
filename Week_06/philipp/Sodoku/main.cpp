#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>
#include <cstdio>
#include <cstring>

#include <Windows.h>

// Defines
#define _OL (char)218
#define _OR (char)191
#define _UL (char)192
#define _UR (char)217
#define _OT (char)193
#define _UT (char)194
#define _LT (char)180
#define _RT (char)195
#define _KR (char)197
#define _HO (char)196
#define _VE (char)179

struct Feld {
    int zahl;
    int start;
};

void outputGrid(int n, Feld sudokus[50][9][9])
{
	std::cout << _OL << std::setfill(_HO);
	std::cout << std::setw(11) << _HO << _UT << std::setw(11) << _HO << _UT << std::setw(11) << _HO;
	std::cout << _OR << std::endl;
	for(int i = 0; i< 9; i++) {
		if(i % 3 == 0 && !(i == 0)) {
			std::cout << _RT;
			std::cout << std::setw(11) << _HO << _KR << std::setw(11) << _HO << _KR << std::setw(11) << _HO;
			std::cout << _LT;
			std::cout << std::endl;
		}
		std::cout << _VE;
		for(int j = 0; j < 9; j++) {
			if(sudokus[n][i][j].start == 1)
				SetConsoleTextAttribute(::GetStdHandle(STD_OUTPUT_HANDLE), 12);
            else if(sudokus[n][i][j].start == 0)
				SetConsoleTextAttribute(::GetStdHandle(STD_OUTPUT_HANDLE), 2);
			if(sudokus[n][i][j].zahl != 0)
				std::cout << " " << sudokus[n][i][j].zahl << " ";
			else
				std::cout << "   ";
			if(sudokus[n][i][j].start == 1 || sudokus[n][i][j].start == 0) 
				SetConsoleTextAttribute(::GetStdHandle(STD_OUTPUT_HANDLE), 7);
			if(j % 3 == 2)
				std::cout << _VE;
			else
				std::cout << " ";
		}
		std::cout << std::endl;
	}
	std::cout << _UL;
	std::cout << std::setw(11) << _HO << _OT << std::setw(11) << _HO << _OT << std::setw(11) << _HO;
	std::cout << _UR << std::endl;
}

bool readFields(std::string const &filename, Feld sudokus[50][9][9], bool printOutput = false)
{
    int n = 0;
    std::ifstream infile(filename);
    std::string grid;
    while(n < 50) {
        getline(infile, grid);
        std::cout << "Reading Grid: " << grid << std::endl;
        for(int i = 0; i < 9; i++) {
            std::string line;
            getline(infile, line);
            for(int j = 0; j < 9; j++) {
                sudokus[n][i][j].zahl = (int)line.at(j) - 48;
                if(((int)line.at(j) - 48) != 0)
                    sudokus[n][i][j].start = 1;
                else
                    sudokus[n][i][j].start = 0;
            }
        }
        if(printOutput)
            outputGrid(n, sudokus);
        n++;
    }
    return true;
}

bool checkRow(int n, Feld sudokus[50][9][9], int row)
{
	bool check[9] = { 0 };
	for(int i = 0; i < 9; i++) {
		Feld wert = sudokus[n][row][i];
		if(wert.zahl == 0)
			continue;
		if(check[wert.zahl - 1] == 1)
			return false;
		else
			check[wert.zahl - 1] = 1;
	}
	return true;
}

bool checkCol(int n, Feld sudokus[50][9][9], int col)
{
	bool check[9] = { 0 };
	for(int i = 0; i < 9; i++) {
		Feld wert = sudokus[n][i][col];
		if(wert.zahl == 0)
			continue;
		if(check[wert.zahl - 1] == 1)
			return false;
		else
			check[wert.zahl - 1] = 1;
	}
	return true;
}

bool checkBox(int n, Feld sudokus[50][9][9], int _row, int _col)
{
	bool check[9] = { 0 };
	int row = _row % 3;
	int col = _col % 3;
	for(int i = row*3; i < row*3 + 3; i++) {
		for(int j = col*3; j < col*3 + 3; j++) {
			Feld wert = sudokus[n][i][j];
			if(wert.zahl == 0)
				continue;
			if(check[wert.zahl - 1] == 1)
				return false;
			else
				check[wert.zahl - 1] = 1;
		}
	}
	return true;
}

void solveByBacktracking(int n, Feld sudokus[50][9][9], int row, int col)
{
	if(row > 8)
		throw ("Solution found");
	while(sudokus[n][row][col].start == 1) {
		col += 1;
		if(col > 8) {
			col = 0;
			row += 1;
		}
		if(row > 8)
			throw ("Solution found");
	}
	for(int i = 1; i <= 9; i++) {
		sudokus[n][row][col].zahl = i;
		if(checkRow(n, sudokus, row) && checkCol(n, sudokus, col) && checkBox(n, sudokus, row, col)) {
			if(col + 1 > 8)
				solveByBacktracking(n, sudokus, row + 1, 0);
			else
				solveByBacktracking(n, sudokus, row, col + 1);
		} else {
			sudokus[n][row][col].zahl = 0;
		}
	}
	sudokus[n][row][col].zahl = 0;
}

void solveByBacktracking(int n, Feld sudokus[50][9][9])
{
	try {
		solveByBacktracking(n, sudokus, 0, 0);
	} catch(...) {

	}
}

int solve(Feld sudokus[50][9][9], bool printGrid = false)
{
    int lsg = 0;
    for(int i = 0; i < 50; i++) {
        std::cout << "Solving Sudoku : " << (i + 1) << std::endl;
        solveByBacktracking(i, sudokus);
        if(printGrid)
            outputGrid(i, sudokus);
        lsg += sudokus[i][0][0].zahl;
        lsg += sudokus[i][1][0].zahl;
        lsg += sudokus[i][2][0].zahl;
    }

    return lsg;
}

int main(int argc, char **argv)
{
    if(argc < 2) {
        std::cerr << "Invalid number of arguments! Expecting at least a sudoku file." << std::endl;
    }
    std::string filename = argv[1];
    bool printOutput = false;
    if(argc == 3 && strcmp("-o", argv[2]) == 0) {
        printOutput = true;
    }

    std::cout << "Filename : " << filename << std::endl;
    std::cout << "Output : " << (printOutput ? "true" : "false") << std::endl;
    Feld sudokus[50][9][9];

    readFields(filename, sudokus, printOutput);

    int lsg = solve(sudokus, printOutput);

    std::cout << "The solution is: " << lsg << std::endl;

    getchar();
    return 0;
}