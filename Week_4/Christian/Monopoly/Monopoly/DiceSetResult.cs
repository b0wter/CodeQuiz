using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class DiceSetResult
    {
        public DiceSetResult(int result, bool xOfAKind)
        {
            XOfAKind = xOfAKind;
            Result = result;
        }

        public bool XOfAKind { get; set; }
        public int Result { get; set; }

    }
}
