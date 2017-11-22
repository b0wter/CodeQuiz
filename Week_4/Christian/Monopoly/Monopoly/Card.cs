using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public abstract class Card
    {
        public Fields GotoField { get; set; }
        public int Counter { get; set; } //Zum Debuggen
    }
}
