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

        public DiceSetResult RollDice(DiceSet diceSet, DiceSetResult resultSet)
        {
            diceSet.RollDiceSet();
            resultSet.AddDiceSetToResult(diceSet);
            return resultSet;
        }
    }
}
