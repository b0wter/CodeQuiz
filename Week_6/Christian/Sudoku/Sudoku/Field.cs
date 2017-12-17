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
        public int FieldValue { get; set; }

        public bool IsFixed { get; set; }
    }
}
