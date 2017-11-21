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

        public int Result { get; set; }
        public bool SameOfAKind { get; set; }
        public Dice[] DiceSetList { get; set; }

        public void RollDiceSet()
        {
            int diceRollResultNew = 0;
            int diceRollResultOld = 0;
            SameOfAKind = false;
            Result = 0;

            foreach(Dice d in DiceSetList)
            {
                diceRollResultNew = d.RollDice();
              
                Result += diceRollResultNew;
                SameOfAKind = (diceRollResultOld > 0 && diceRollResultNew == diceRollResultOld) ? true : false;

                diceRollResultOld = diceRollResultNew;
            }                   
        }
    }
}
