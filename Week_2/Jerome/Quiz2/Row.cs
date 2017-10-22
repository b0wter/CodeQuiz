using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Quiz2
{
    class Row
    {

        public Row()
        {
            Entries = new List<Entry>();
        }

        public int RowNumber { get; set; }
        public List<Entry> Entries { get; set; }
    }
}
