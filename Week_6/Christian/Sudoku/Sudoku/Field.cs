using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sudoku
{
    [Serializable]
    public class Field
    {
        public Field(int fieldValue, bool isFixed)
        {
            FieldValue = fieldValue;
            IsFixed = isFixed;
            InitializePossibleValues();
        }

        public int FieldValue { get; set; }

        public bool IsFixed { get; set; }

        public HashSet<int> PossibleValues { get; set; }

        public void InitializePossibleValues()
        {
            PossibleValues = new HashSet<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

            if (IsFixed)
            {
                PossibleValues.Remove(FieldValue);
            }
        }
    }
}
