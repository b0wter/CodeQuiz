using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Api
{
    public abstract class BaseEntity
    {
       public string Name { get; set; }
       public UInt64 Id { get; set; }
    }
}
