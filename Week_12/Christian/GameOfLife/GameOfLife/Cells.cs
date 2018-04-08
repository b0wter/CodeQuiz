using System;
using System.Collections.Generic;
using System.Text;

namespace GameOfLife
{
    public class Cells
    {
        private int _numberOfCellsX;
        private int _numberOfCellsY;
        public Cells(int numberOfCellsX, int numberOfCellsY)
        {
            CellsArray = new Cell[numberOfCellsY, numberOfCellsX];
            _numberOfCellsX = numberOfCellsX;
            _numberOfCellsY = numberOfCellsY;
            InitializeCells();
        }

        public Cell[,] CellsArray { get; set; }

        private void InitializeCells()
        {
            Random r = new Random();

            for (int y = 0; y < _numberOfCellsY; y++)
            {
                for (int x = 0; x < _numberOfCellsX; x++)
                {
                    CellsArray[y, x] = new Cell() { CurrentState = r.Next(1, 3) == 1 ? CellState.Alive : CellState.Dead, FutureState = CellState.Undefined };
                }
            }
        }

        private int AliveNeighbourCount(Cell cell, int aliveNeighbours)
        {
            if (cell.CurrentState == CellState.Alive)
            {
                aliveNeighbours++;
            }

            return aliveNeighbours;
        }

        private void SetNewCellStates()
        {
            for (int y = 0; y < _numberOfCellsY; y++)
            {
                for (int x = 0; x < _numberOfCellsX; x++)
                {
                    CellsArray[y, x].MoveToNextState();
                }
            }
        }

        public void MoveToNextState()
        {
            for (int y = 0; y < _numberOfCellsY; y++)
            {
                for (int x = 0; x < _numberOfCellsX; x++)
                {

                    int aliveNeighbours = 0;

                    int yL = y - 1;
                    if (yL >= 0)
                    {
                        //above
                        if (x - 1 > -1)
                        {
                            aliveNeighbours = AliveNeighbourCount(CellsArray[yL, x - 1], aliveNeighbours);
                        }

                        aliveNeighbours = AliveNeighbourCount(CellsArray[yL, x], aliveNeighbours);


                        if (x + 1 < _numberOfCellsX)
                        {
                            aliveNeighbours = AliveNeighbourCount(CellsArray[yL, x + 1], aliveNeighbours);
                        }
                    }

                    //same row
                    if (x - 1 > -1)
                    {
                        aliveNeighbours = AliveNeighbourCount(CellsArray[y, x - 1], aliveNeighbours);
                    }

                    if (x + 1 < _numberOfCellsX)
                    {
                        aliveNeighbours = AliveNeighbourCount(CellsArray[y, x + 1], aliveNeighbours);
                    }

                    //below
                    int yH = y + 1;
                    if (yH < _numberOfCellsY)
                    {
                        if (x - 1 > -1)
                        {
                            aliveNeighbours = AliveNeighbourCount(CellsArray[yH, x - 1], aliveNeighbours);
                        }

                        aliveNeighbours = AliveNeighbourCount(CellsArray[yH, x], aliveNeighbours);

                        if (x + 1 < _numberOfCellsX)
                        {
                            aliveNeighbours = AliveNeighbourCount(CellsArray[yH, x + 1], aliveNeighbours);
                        }
                    }

                    CellsArray[y, x].FutureState = ApplyRules(CellsArray[y, x], aliveNeighbours);
                }
            }

            SetNewCellStates();
        }

        //Jede lebendige Zelle mit weniger als zwei lebenden Nachbarn stirbt an Einsamkeit.
        //Jede lebendige Zelle mit zwei, oder drei lebenden Nachbarn lebt auch im nächsten Zeitschritt.
        //Jede lebendige Zelle mit vier, oder mehr lebenden Nachbarn stirbt an Überbevölkerung.
        //Jede tote Zelle mit genau drei lebendigen Nachbarn lebt im nächsten Schritt.
        public CellState ApplyRules(Cell cell, int numberOfAliveNeighbours)
        {
            if (cell.CurrentState == CellState.Alive)
            {
                if (numberOfAliveNeighbours < 2)
                {
                    return CellState.Dead;
                }

                if (numberOfAliveNeighbours == 2 || numberOfAliveNeighbours == 3)
                {
                    return CellState.Alive;
                }

                if (numberOfAliveNeighbours > 3)
                {
                    return CellState.Dead;
                }
            }

            if (cell.CurrentState == CellState.Dead)
            {
                if (numberOfAliveNeighbours == 3)
                {
                    return CellState.Alive;
                }
            }

            return CellState.Dead;
        }

        public bool OneCellIsStillAlive()
        {
            for (int y = 0; y < _numberOfCellsY; y++)
            {
                for (int x = 0; x < _numberOfCellsX; x++)
                {
                    if (CellsArray[y, x].CurrentState == CellState.Alive)
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        public override string ToString()
        {
            StringBuilder stringBuilder = new StringBuilder();

            for (int y = 0; y < _numberOfCellsY; y++)
            {
                for (int x = 0; x < _numberOfCellsX; x++)
                {
                    stringBuilder.Append(CellsArray[y, x].ToString());
                }

                stringBuilder.Append("\n");
            }

            return stringBuilder.ToString();
        }
    }
}
