using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{    
    public class DiceSet
    {
        public DiceSet(Dice[] diceSet)
        {
            DiceSetList = diceSet;
        }

        public Dice[] DiceSetList { get; set; }

        public int RollDiceSet()
        {
            int result = 0;
            foreach(Dice d in DiceSetList)
            {
                result += d.RollDice();
            }

            return result;
        }
    }
}
