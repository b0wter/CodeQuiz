using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sudoku
{

    public sealed class SudokuFieldProvider
    {
        private static int _counter;
        private static List<LargeSquare> _largeSquares;
        private static List<LargeSquare> _solutionList;

        private static readonly Lazy<SudokuFieldProvider> lazy =
            new Lazy<SudokuFieldProvider>(() => new SudokuFieldProvider());

        public static SudokuFieldProvider Instance { get { return lazy.Value; } }


        private SudokuFieldProvider()
        {
            _solutionList = new List<LargeSquare>();
        }

        public LargeSquare GetNextSudokuField()
        {

            if (_counter >= _largeSquares.Count())
            {
                return null;
            }

            LargeSquare largeSquare = _largeSquares.ElementAt(_counter);

            _counter++;

            return largeSquare;
        }

        public void AddResultToList(LargeSquare largeSquare)
        {
            _solutionList.Add(largeSquare);
        }

        public void SetInitialList(List<LargeSquare> largeSquareListe)
        {
            _largeSquares = largeSquareListe;
        }

        public List<LargeSquare> GetResultList()
        {
            return _solutionList;
        }
    }
}
