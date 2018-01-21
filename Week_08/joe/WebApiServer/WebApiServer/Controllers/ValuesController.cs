using System;
using System.Collections.Generic;
using System.Dynamic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using WebApiServer.Data;

namespace WebApiServer.Controllers
{
    [Route("api/[controller]")]
    public class ValuesController : Controller
    {
        private readonly IDataProvider _provider;

        public ValuesController(IDataProvider provider)
        {
            _provider = provider;
        }
        
        [HttpGet]
        public string Get()
        {
            var result = _provider.FilterByProperties(IQueryCollectionToDictionary(Request.Query));
            return result.ToString();
        }

        private Dictionary<string, string> IQueryCollectionToDictionary(IQueryCollection query)
        {
            var dict = new Dictionary<string, string>();
            foreach (var parameter in query)
                dict.Add(parameter.Key, parameter.Value);

            return dict;
        }
    }
}
