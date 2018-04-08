using System;

namespace GameOfLife
{
    class Program
    {
        static void Main(string[] args)
        {

            Cells cells = new Cells(50, 50);
            Console.WindowHeight = Console.LargestWindowHeight;
            Console.WindowWidth = Console.LargestWindowWidth;
            int round = 0;

            while (cells.OneCellIsStillAlive())
            {
                cells.MoveToNextState();
                Console.Clear();
                Console.Write(cells.ToString());
                round++;
                Console.WriteLine($"Round: {round}");
                System.Threading.Thread.Sleep(500);
            }

            Console.WriteLine("All died!");
            Console.ReadLine();
        }
    }
}
