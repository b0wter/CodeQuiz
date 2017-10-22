using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Quiz2
{
    class Pyramid
    {
        public List<Row> Rows { get; set; }


        public Pyramid()
        {
            Rows = new List<Row>();
        }


        public void LinkEntries()
        {
            foreach(Row row in Rows)
            {
                int count = 0;

                if(row.RowNumber < Rows.Count-1)
                {
                    foreach (Entry entry in row.Entries)
                    {
                        entry.Left = Rows[row.RowNumber + 1].Entries[count];
                        entry.Right = Rows[row.RowNumber + 1].Entries[count + 1];
                        count++;
                    }
                }
            }
        }


        public int getMaxPathSum()
        {

            for (int i = Rows.Count - 2; i >= 0; i--)
            {
                foreach (Entry entry in Rows[i].Entries)
                {
                    entry.Value = Math.Max(entry.Value + entry.Left.Value, entry.Value + entry.Right.Value);
                }
            }
            return Rows[0].Entries[0].Value;
        }
    }
}
