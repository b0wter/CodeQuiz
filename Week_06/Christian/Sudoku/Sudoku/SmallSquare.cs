using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.Serialization.Formatters.Binary;
using System.IO;

namespace Sudoku
{
    [Serializable]
    public class SmallSquare
    {
        public SmallSquare()
        {
            Fields = new Field[3,3];
        }
        public Field[,] Fields { get; set; } 
     

        public bool IsValid()
        {
            HashSet<int> alreadyUsedNumbers = new HashSet<int>();

            for (int x=0; x<3; x++)
            {
                for (int y=0; y<3; y++)
                {
                    Field field = Fields[x, y];
                    if (field.FieldValue > 0)
                    {
                        if (alreadyUsedNumbers.Contains(field.FieldValue))
                        {
                            return false;
                        }
                        else
                        {
                            alreadyUsedNumbers.Add(field.FieldValue);
                        }
                    }
                }
            }

            return true;
        }

        public bool IsComplete()
        {
            for (int x = 0; x < 3; x++)
            {
                for (int y = 0; y < 3; y++)
                {
                    Field field = Fields[x, y];
                    if (field.FieldValue == 0)
                    {
                        return false;
                    }
                }
            }

            return true;
        }

        public HashSet<int> GetNumbersStillAvailable()
        {
            HashSet<int> availableNumbers = new HashSet<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

            for (int x = 0; x < 3; x++)
            {
                for (int y = 0; y < 3; y++)
                {
                    Field field = Fields[x, y];
                    if (availableNumbers.Contains(field.FieldValue))
                    {
                        availableNumbers.Remove(field.FieldValue);
                    }
                }
            }

            return availableNumbers;
        }

        public List<SmallSquare> GetAllPossibleSquares(List<SmallSquare> listOfSquares, int xPosition, int yPosition, HashSet<string> invalidFields, int smallSquareNumber)
        { 
            int elementCounter = 0;

            if (listOfSquares.Count == 0)
            {
                listOfSquares.Add(this);
            }

            if (yPosition == 3)
            {
                System.Diagnostics.Debug.WriteLine("GetAllPossibleSquares return");                

                Dictionary<string, SmallSquare> newList = new Dictionary<string, SmallSquare>();

                foreach (SmallSquare sq in listOfSquares)
                {
                    bool isOK = true;
                    if (smallSquareNumber < 6)
                    {
                        for (int y = 0; y < 3; y++)
                        {
                            for (int x = 0; x < 3; x++)
                            {
                                if (!sq.Fields[y, x].IsFixed)
                                {
                                      if (invalidFields.Contains(SudokuSolver.GetBlockListEntry(smallSquareNumber, sq.Fields[y, x].FieldValue, x, y)))
                                    {
                                        isOK = false;
                                        break;
                                    }
                                }

                                if (!isOK)
                                {
                                    break;
                                }
                            }
                        }
                    }
                    if (isOK && sq.IsComplete())
                    {
                        if (!newList.ContainsKey(sq.GetKey()))
                        {
                            newList.Add(sq.GetKey(), sq);
                        }

                    }
                }

                if (!newList.Any())
                {
                    System.Diagnostics.Debug.WriteLine("no results");
                }             

                return newList.Select(val => val.Value).ToList();
            }

            Dictionary<string, SmallSquare> newSquares = new Dictionary<string, SmallSquare>();

            foreach (SmallSquare existingSquare in listOfSquares)
            {
                elementCounter = 0;

                for (int x = 0; x < 9; x++)
                {
                    SmallSquare newSquare = Program.DeepClone(existingSquare);
                    HashSet<int> availableNumbers = newSquare.GetNumbersStillAvailable();

                    if (newSquare.Fields[yPosition, xPosition].IsFixed == false)
                    {
                        newSquare.Fields[yPosition, xPosition].FieldValue = availableNumbers.ElementAt(elementCounter);
                        elementCounter++;
                    }

                    if (!newSquares.ContainsKey(newSquare.GetKey()))
                    {
                        newSquares.Add(newSquare.GetKey(), newSquare);
                    }


                    if (availableNumbers.Count() == elementCounter)
                    {
                        break;
                    }
                }
            }

            if (xPosition == 2)
            {
                xPosition = 0;
                yPosition++;
            }
            else
            {
                xPosition++;
            }

            return GetAllPossibleSquares(newSquares.Select(val => val.Value).ToList(), xPosition, yPosition, invalidFields, smallSquareNumber);
        }

        public override string ToString()
        {
            StringBuilder stringBuilder = new StringBuilder();

            for (int y=0; y<3; y++)
            {
                stringBuilder.Append($"Y{y}:");
                for (int x=0; x<3; x++)
                {
                    stringBuilder.Append($" {Fields[y,x].FieldValue} ");
                }

                stringBuilder.Append(" /n ");
            }
                return stringBuilder.ToString();
        }


        private string GetKey()
        {
            return ($"{Fields[0,0].FieldValue}_{Fields[0, 1].FieldValue}_{Fields[0, 2].FieldValue}_{Fields[1, 0].FieldValue}_{Fields[1, 1].FieldValue}_{Fields[1, 2].FieldValue}_{Fields[2, 0].FieldValue}_{Fields[2, 1].FieldValue}_{Fields[2, 2].FieldValue}");
        }

    }
}
