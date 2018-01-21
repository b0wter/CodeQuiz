using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Sudoku
{
    class Program
    {
        
        static void Main(string[] args)
        {                        
            SudokuFieldProvider sudokuFieldProvider = SudokuFieldProvider.Instance;
            sudokuFieldProvider.SetInitialList(InitializeSquares(ReadFile()));
            SudokuSolver c = new SudokuSolver(sudokuFieldProvider);
            
            List<Thread> allThreads = new List<Thread>();

            for (int x = 0; x < Environment.ProcessorCount; x++)
            {
                Thread thread = new Thread(new ThreadStart(c.GetSolutions));
                thread.Start();
                allThreads.Add(thread);
            }

            foreach (Thread thread in allThreads)
            {
                thread.Join();
            }

            WriteResult(sudokuFieldProvider.GetResultList());

            Console.ReadLine();
        }

        private static void WriteResult(List<LargeSquare> list)
        {
            using (System.IO.StreamWriter file =
           new System.IO.StreamWriter(@"C:\temp\SudokuResult.txt"))
            {
                foreach (LargeSquare lsSol in list)
                {
                    file.WriteLine(lsSol.ToString());
                    file.WriteLine(lsSol.GetResult());
                    file.WriteLine(" ");

                }

            }
        }

        private static string[] ReadFile()
        {
           return File.ReadAllLines(@"C:\temp\sudoku.txt");
        }

        private static List<LargeSquare> InitializeSquares(string[] fileInput)
        {
            List<LargeSquare> list = new List<LargeSquare>();

            LargeSquare largeSquare = new LargeSquare();
            List<string> lines = new List<string>();
            int gridCounter = 0;

            foreach(string line in fileInput)
            {
                if (line.StartsWith("Grid", StringComparison.OrdinalIgnoreCase))
                {
                    largeSquare = new LargeSquare();
                    largeSquare.Name = $"Grid {gridCounter.ToString().PadLeft(2, '0')}";
                    gridCounter++;
                    if (lines.Count > 0)
                    {
                        //initialise
                        largeSquare.Initialize(lines.ToArray());
                        list.Add(largeSquare);
                        lines = new List<string>();
                    }                    
                }
                else
                {
                    lines.Add(line);
                }
            }

            //initialize last
            largeSquare = new LargeSquare();
            largeSquare.Initialize(lines.ToArray());
            largeSquare.Name = $"Grid {gridCounter.ToString().PadLeft(2, '0')}";
            list.Add(largeSquare);

            return list;
        }

      
       

        public static T DeepClone<T>(T obj)
        {
            using (var ms = new MemoryStream())
            {
                var formatter = new BinaryFormatter();
                formatter.Serialize(ms, obj);
                ms.Position = 0;

                return (T)formatter.Deserialize(ms);
            }
        }
    }
}
