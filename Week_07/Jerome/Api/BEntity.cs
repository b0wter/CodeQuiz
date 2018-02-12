using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Api
{
    public class BEntity : BaseEntity
    {
        public bool IsAwesome { get; set; }
        public bool IsTehSuck { get; set; }
        public uint CEntityId { get; set; }
        public CEntity CEntityDetails { get; set; }
    }
}
