using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using VoterApi.Model;
using VoterApi.DataAccess;

namespace VoterApi.Controllers
{
    [Route("api/[controller]")]
    public class VoterController : Controller
    {
        public const string fileName = "c:\\temp\\voter.csv";
        
            
        [Route("{county?}/{limit?}/{month?}/{active?}/{party?}")]
        [HttpGet("{county}")]
        public IEnumerable<Voter> Get(string county = null, int? limit = null, int? month = null)
        {
            VoterFileAccess fileAccess = new VoterFileAccess();
            List<Voter> voterList = fileAccess.GetVoterListFromFile(fileName).ToList();
            
            if (!string.IsNullOrEmpty(county))
            {
                voterList = voterList.Where(vot => vot.County.Equals(county, StringComparison.OrdinalIgnoreCase)).ToList();
            }

            if (month != null)
            {
                voterList = voterList.Where(vot => vot.Date.Month == month).ToList();
            }            

            if (limit != null)
            {
                voterList = voterList.Take((int)limit).ToList();
            }
         
            return voterList;
        }
      
        //Below not used
        // POST api/values
        [HttpPost]
        public void Post([FromBody]string value)
        {
        }

        // PUT api/values/5
        [HttpPut("{id}")]
        public void Put(int id, [FromBody]string value)
        {
        }

        // DELETE api/values/5
        [HttpDelete("{id}")]
        public void Delete(int id)
        {
        }

        

        

    }
}
