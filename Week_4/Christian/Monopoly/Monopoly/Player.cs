using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class Player
    {
        private Fields _currentPosition;

        public Player()
        {
            _currentPosition = Fields.GO;
        }

        public void SetPosition(Fields newField)
        {
            _currentPosition = newField;
            //Console.WriteLine(String.Format("New Position: {0}", _currentPosition));
        }

        public Fields GetPosition()
        {
            return _currentPosition;
        }

        public DiceSetResult RollDice(DiceSet diceSet)
        {
            int diceRollResultNew = 0;
            int diceRollResultOld = 0;
            int result = 0;
            bool xOfAKind = false;

            foreach(Dice d in diceSet.DiceSetList)
            {
                diceRollResultNew = d.RollDice();
                result += diceRollResultNew;
                xOfAKind = (diceRollResultOld > 0 && diceRollResultNew == diceRollResultOld) ? true : false;
              
                diceRollResultOld = diceRollResultNew;
            }

            return new DiceSetResult(result, xOfAKind);
        }
    }
}
