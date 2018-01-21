using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace WebApiServer.Data
{
    public interface IDataProvider
    {
        JArray FilterByProperties(Dictionary<string, string> filters);
    }
}
