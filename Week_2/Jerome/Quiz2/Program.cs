using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Quiz2
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Bitte den Pfad zu der Pyramiden-Datei angeben");

            var path = Console.ReadLine();

            string[] stringArray = System.IO.File.ReadAllLines(@path);

            Pyramid pyramid = BuildPyramid(stringArray);

            Console.WriteLine($"Die größte Summe beträgt {pyramid.getMaxPathSum().ToString()}");

            Console.ReadKey();
        }

        public static Pyramid BuildPyramid(string[] input)
        {
            Pyramid newPyramid = new Pyramid
            {
                Rows = FillRowList(input)
            };

            newPyramid.LinkEntries();

            return newPyramid;
        }

        public static List<Row> FillRowList(string[] input)
        {
            List<Row> rows = new List<Row>();

            int rowCount = 0;

            foreach(string entriesString in input)
            {
                var entries = entriesString.Split(' ');

                rows.Add(new Row { RowNumber = rowCount });

                foreach(string entry in entries)
                {
                    rows.Last().Entries.Add(new Entry { Value = Int32.Parse(entry) });
                }
                rowCount++;
            }

            return rows;
        }        
    }
}