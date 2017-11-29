using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class DiceSetResult
    {
        private int _totalResult;
        private int _totalNumberXofAKind;


        public DiceSetResult()
        {         
        }

        public DiceSet LastDiceSet { get; set; }

        public void AddDiceSetToResult(DiceSet diceSet)
        {
            _totalResult += diceSet.Result;

            if (diceSet.SameOfAKind)
            {
                _totalNumberXofAKind++;
            }

            LastDiceSet = diceSet;
        }

        public int GetTotal()
        {
            return _totalResult;
        }

        public int GetTotalNumberXOfAKind()
        {
            return _totalNumberXofAKind;
        }

        

    }
}
