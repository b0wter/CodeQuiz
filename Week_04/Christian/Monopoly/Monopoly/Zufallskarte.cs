using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class Zufallskarte : Card
    {
        public SpecialAction SpecialAction { get; set; }
    }

    public enum SpecialAction
    {
        None = 0,
        MoveToNextR = 1,
        MoveToNextU = 2,
        MoveThreeFieldsBack = 3
    }
}
