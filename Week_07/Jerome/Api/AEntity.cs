using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Api
{
    public class AEntity : BaseEntity
    {
        public int Max { get; set; }
        public int Min { get; set; }
        public uint CEntityId { get; set; }
        public CEntity CEntityDetails { get; set; }
    }
}
