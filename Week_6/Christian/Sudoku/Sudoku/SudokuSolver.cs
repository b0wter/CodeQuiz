using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sudoku
{
    public class SudokuSolver
    {
        private SudokuFieldProvider _sudokuFieldProvider;
        private HashSet<string> _invalidFields;
        public SudokuSolver(SudokuFieldProvider sudokuFieldProvider)
        {
            _sudokuFieldProvider = sudokuFieldProvider;
        }

        public void GetSolutions()
        {
            LargeSquare largeSquare = null;
            do
            {
                lock (_sudokuFieldProvider)
                {
                    largeSquare = _sudokuFieldProvider.GetNextSudokuField();
                }

                if (largeSquare != null)
                {
                    System.Diagnostics.Debug.WriteLine($"Working on {largeSquare.Name}");

                    List<LargeSquare> allPossibilities = new List<LargeSquare>();
                    GetInvalidFields(largeSquare);

                    allPossibilities = largeSquare.GetAllCombinations(allPossibilities, 0, _invalidFields);

                    foreach (LargeSquare lsSol in allPossibilities)
                    {
                        if (lsSol.IsValid())
                        {
                            lock (_sudokuFieldProvider)
                            {
                                _sudokuFieldProvider.AddResultToList(lsSol);
                            }

                            Console.WriteLine(lsSol.ToString());
                            Console.WriteLine(lsSol.GetResult());
                            Console.WriteLine(" ");
                        }
                    }
                }
            } while (largeSquare != null);
        }

        private int GetSquareNumber(int x, int y)
        {
             switch (x)
             {
                 case 0:
                 case 1:
                 case 2:
                     if (y < 3)
                     {
                         return 0;
                     }
                     else if (y < 5)
                     {
                         return 1; 
                     }
                     return 2;

                 case 3:
                 case 4:
                 case 5:
                     if (y < 3)
                     {
                         return 3; 
                     }
                     else if (y < 5)
                     {
                         return 4;
                     }
                     return 5; 

                 case 6:
                 case 7:
                 case 8:
                     if (y < 3)
                     {
                         return 6; 
                     }
                     else if (y < 5)
                     {
                         return 7;
                     }
                     return 8; 
             }

            return -1;
        }

        public void GetInvalidFields(LargeSquare largeSquare)
        {
            _invalidFields = new HashSet<string>();
           
            for (int y = 0; y < 9; y++)
            {
                for (int x = 0; x < 9; x++)
                {                   

                    Field field = largeSquare.Fields[y, x];
                    if (field.IsFixed)
                    {
                        //Does not work completely :-(
                        //AddVerticalBlocker(GetSquareNumber(x,y), field.FieldValue, x, y);
                        //AddHorizontalBlocker(GetSquareNumber(x, y), field.FieldValue, x, y);
                    }
                }
            }
       
        }

        public static string GetBlockListEntry(int squareNumber, int blockNumber, int x, int y)
        {
            return $"{squareNumber}_{blockNumber}_{x}_{y}";
        }
        private void AddVerticalBlocker(int squareNumber, int blockNumber, int x, int y)
        {
            int startSquare = 0;
            int xCorrection = 0;
            
            switch(squareNumber)
            {
                case 0:
                case 1:
                case 2: startSquare = 0; break;

                case 3:
                case 4:
                case 5: startSquare = 1; break; 

                case 6:
                case 7:
                case 8: startSquare = 2; break;

            }

            switch (x)
            {
                case 0:
                case 1:
                case 2: xCorrection = 0; break; 
                case 3:
                case 4:
                case 5: xCorrection = 3; break; 
                case 6:
                case 7:
                case 8: xCorrection = 6; break; 
            }

            for (int j = 0; j < 3; j++)
            {
                for (int i = 0; i < 3; i++)
                {
                     _invalidFields.Add(GetBlockListEntry(startSquare, blockNumber, (x - xCorrection), i));                    
                }

                startSquare = startSquare + 3;
            }           
            
        }

        private void AddHorizontalBlocker(int squareNumber, int blockNumber, int x, int y)
        {
            int startSquare = 0;
            int yCorrection = 0;

            if (y > 5)
            {
                yCorrection = 6;
            }
            else if (y > 2)
            {
                yCorrection = 3;
            }           

            switch (squareNumber)
            {
                case 0:
                case 3:
                case 6: startSquare = 0; break; 

                case 1:
                case 4:
                case 7: startSquare = 3; break; 

                case 2:
                case 5:
                case 8: startSquare = 6; break; 
            }          
           
            for (int j = 0; j< 3; j++)
            {
                for (int i = 0; i < 3; i++)
                {
                    _invalidFields.Add(GetBlockListEntry(startSquare, blockNumber, i, (y - yCorrection)));
                }

                startSquare++ ;
            }
        }
    }


}
