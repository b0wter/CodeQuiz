using System;
using System.Collections.Generic;
using System.Text;

namespace GameOfLife
{
    public class Cell
    {
        public CellState CurrentState { get; set; }
        public CellState FutureState { get; set; }


        public void MoveToNextState()
        {
            if (FutureState == CellState.Undefined)
            {
                throw new ApplicationException("Cell state is undefined. Transition into next phase not possible.");
            }

            CurrentState = FutureState;
            FutureState = CellState.Undefined;
        }

        public override string ToString()
        {
            return CurrentState == CellState.Undefined ? "?" : (CurrentState == CellState.Dead ? Convert.ToChar(0x2588) : ' ').ToString();
        }
    }

    public enum CellState
    {
        Undefined = -1,
        Dead = 0,
        Alive = 1
    }
}
