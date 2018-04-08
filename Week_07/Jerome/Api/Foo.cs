using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Api
{
    class Foo : BaseEntity
    {
        public bool IsClosed { get; set; }
        public List<uint> ChildIds { get; set; }      // Liste mit den Ids der Kindelemnte (entweder A- oder BEntity)
        public CEntity Details { get; set; }

    }
}
