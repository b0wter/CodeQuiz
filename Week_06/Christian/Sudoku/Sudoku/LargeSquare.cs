using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sudoku
{
    [Serializable]
    public class LargeSquare
    {
        public LargeSquare()
        {            
            Fields = new Field[9, 9];

        }

        public Field[,] Fields { get; set; }

        public string Name { get; set; }

        /// <summary>
        /// Example:
        ///003020600
        ///900305001
        ///001806400
        ///008102900
        ///700000008
        ///006708200
        ///002609500
        ///800203009
        ///005010300
        /// </summary>
        /// <param name="fieldData"></param>
        public void Initialize(string[] fieldData)
        {
            for (int x = 0; x < 9; x++)
            {
                int[] lineData = GetIntArrayForLine(fieldData[x]);

                for (int y = 0; y < 9; y++)
                {
                    if (lineData[y] > 0)
                    {
                        SetFixedValue(x, y, lineData[y]);
                    }
                    else
                    {
                        SetValue(x, y, lineData[y]);
                    }
                }
            }
        }       

        private int[] GetIntArrayForLine(string line)
        {
            List<int> intList = new List<int>();
            for (int x= 0; x<9; x++)
            {
                intList.Add(Convert.ToInt32(Char.GetNumericValue(line[x])));
            }

            return intList.ToArray();
        }

        public void SetFixedValue(int x, int y, int value)
        {
            Field field = new Field(value, true);
            SetField(field, x, y);
        }

        public void SetValue(int x, int y, int value)
        {
            Field field = new Field(value, false);
            SetField(field, x, y);
        }

        private void SetField(Field field, int x, int y)
        {
            Fields[x, y] = field;
        }

        public bool IsPartlyValid() 
        {
            if (!CheckHorizontalLineValidity())
            {
                return false;
            }
            //check vertical lines
            if (!CheckVerticalLineValidity())
            {
                return false;
            }

            return true;
        }

        public bool IsValid() //check rules
        {
            //check sub squares
            if (!CheckSubSquareValidity())
            {
                return false;
            }
            //check horizontal lines
            if (!CheckHorizontalLineValidity())
            {
                return false;
            }
            //check vertical lines
            if (!CheckVerticalLineValidity())
            {
                return false;
            }

            return true;
        }

        private bool CheckSubSquareValidity()
        {
            for (int x=0; x<9; x++)
            {
                SmallSquare smallSquare = GetSquare(x);

                if (!smallSquare.IsValid() || !smallSquare.IsComplete())
                {
                    return false;
                }                
            }

            return true;
        }

        private bool CheckHorizontalLineValidity()
        {
            for(int x=0; x<9;x++)
            {
                HashSet<int> alreadyUsedNumbers = new HashSet<int>();
                for (int y=0; y<9; y++)
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

        private bool CheckVerticalLineValidity()
        {          
            for (int y = 0; y < 9; y++)
            {
                HashSet<int> alreadyUsedNumbers = new HashSet<int>();

                for (int x = 0; x < 9; x++)
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

        private void GetOffSet(int position, out int xOffset, out int yOffset)
        {
            xOffset = 0;
            yOffset = 0;

            switch (position)
            {               
                case 0: xOffset = 0; yOffset = 0; break; //not necessary, just for completeness
                case 1: xOffset = 0; yOffset = 3; break;
                case 2: xOffset = 0; yOffset = 6; break;
                case 3: xOffset = 3; yOffset = 0; break;
                case 4: xOffset = 3; yOffset = 3; break;
                case 5: xOffset = 3; yOffset = 6; break;
                case 6: xOffset = 6; yOffset = 0; break;
                case 7: xOffset = 6; yOffset = 3; break;
                case 8: xOffset = 6; yOffset = 6; break;
                default: break;
            }
        }
        private SmallSquare GetSquare(int position)
        {
            SmallSquare smallSquare = new SmallSquare();
            int xOffset = 0;
            int yOffset = 0;

            GetOffSet(position, out xOffset, out yOffset);

            for (int x=0; x<3; x++)
            {
                for (int y=0; y<3; y++)
                {
                    smallSquare.Fields[x, y] = Fields[x + xOffset, y + yOffset];
                }
            }

            return smallSquare;
        }

        public bool IsComplete()
        {
            for (int x=0; x<9; x++)
            {
                SmallSquare smallSquare = GetSquare(x);
                if (!smallSquare.IsComplete())
                {
                    return false; 
                }
            }

            return true;
        }


        public List<LargeSquare> GetAllCombinations(List<LargeSquare> squareList, int smallSquareCounter, HashSet<string> invalidFields)
        {
            if (smallSquareCounter == -1)
            {
                return squareList;
            }

            if (smallSquareCounter == 0)
            {
                squareList.Add(this);
            }

            SmallSquare smallSquare = GetSquare(smallSquareCounter);
            List<SmallSquare> smallSquareList = new List<SmallSquare>();

            smallSquareList = smallSquare.GetAllPossibleSquares(smallSquareList, 0, 0, invalidFields, smallSquareCounter);
            List<LargeSquare> newLargeSquares = new List<LargeSquare>();
                  
            foreach (LargeSquare existingSquare in squareList)
            {
                foreach (SmallSquare s in smallSquareList)
                {
                    LargeSquare newSquare = Program.DeepClone(existingSquare);

                    newSquare.SetValuesForSquare(s, smallSquareCounter);
                 

                    if (newSquare.IsPartlyValid())
                    {
                        if (smallSquareCounter < 8)
                        {                            
                            newLargeSquares.Add(newSquare);
                        }
                        else
                        {
                            if (newSquare.IsValid())
                            {                              
                                newLargeSquares.Add(newSquare);
                                System.Diagnostics.Debug.WriteLine($"Finished {newSquare.Name}");
                                return newLargeSquares; //since we can only have on result, we can exist as soon as we have found one
                            }
                        }
                    }
                }             
            }

            smallSquareCounter++;

            if (smallSquareCounter == 9)
            {
                smallSquareCounter = -1;
            }
            return GetAllCombinations(newLargeSquares, smallSquareCounter, invalidFields);
        }

        private void SetValuesForSquare(SmallSquare smallSquare, int squareNumber)
        {
            int xOffset = 0;
            int yOffset = 0;

            GetOffSet(squareNumber, out xOffset, out yOffset);

            for (int x = 0; x < 3; x++)
            {
                for (int y = 0; y < 3; y++)
                {
                    Fields[x + xOffset, y + yOffset] = smallSquare.Fields[x, y];
                }
            }
        }
      
        public override string ToString()
        {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.Append($"{Name} \n");
            for (int y = 0; y < 9; y++)
            {
                for (int x = 0; x < 9; x++)
                {
                    if (x > 0 && x % 3 == 0)
                    {
                        stringBuilder.Append("|");
                    }

                    stringBuilder.Append(Fields[y,x].FieldValue);
                    
                    stringBuilder.Append(" ");
                   
                }

                stringBuilder.Append("||\n");
            }

            return stringBuilder.ToString();

        }

        public string GetResult()
        {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.Append($"{Fields[0, 0].FieldValue} + {Fields[1, 0].FieldValue} + {Fields[2, 0].FieldValue} = ");
            stringBuilder.Append($"{Fields[0, 0].FieldValue + Fields[1, 0].FieldValue + Fields[2, 0].FieldValue}");
            return stringBuilder.ToString();
        }


        private string GetKey()
        {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.Append($"{Fields[0, 0].FieldValue}_{Fields[0, 1].FieldValue}_{Fields[0, 2].FieldValue}_{Fields[1, 0].FieldValue}_{Fields[1, 1].FieldValue}_{Fields[1, 2].FieldValue}_{Fields[2, 0].FieldValue}_{Fields[2, 1].FieldValue}_{Fields[2, 2].FieldValue}_");
            stringBuilder.Append($"{Fields[3, 0].FieldValue}_{Fields[3, 1].FieldValue}_{Fields[3, 2].FieldValue}_{Fields[4, 0].FieldValue}_{Fields[4, 1].FieldValue}_{Fields[4, 2].FieldValue}_{Fields[5, 0].FieldValue}_{Fields[5, 1].FieldValue}_{Fields[5, 2].FieldValue}_");
            stringBuilder.Append($"{Fields[6, 0].FieldValue}_{Fields[6, 1].FieldValue}_{Fields[6, 2].FieldValue}_{Fields[7, 0].FieldValue}_{Fields[7, 1].FieldValue}_{Fields[7, 2].FieldValue}_{Fields[8, 0].FieldValue}_{Fields[8, 1].FieldValue}_{Fields[8, 2].FieldValue}");
            return stringBuilder.ToString();
        }
    }
}
