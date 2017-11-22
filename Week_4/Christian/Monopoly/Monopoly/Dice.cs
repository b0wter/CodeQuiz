using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class Dice
    {
        private Random _random;
        private int _number;
        public Dice(int number, Random random)
        {
            _number = number;
            _random = random;
        }

        public int RollDice()
        {
            return _random.Next(1, _number+1);
        }
    }
}
