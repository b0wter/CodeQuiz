using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Net.Http;
using Newtonsoft.Json;



namespace Api
{
    class Program
    {
        static string FooEntityPath = "Foo/";
        static string AEntityPath = "AEntity/";
        static string BEntityPath = "BEntity/";
        static string CEntityPath = "/CEntity";


        static HttpClient client = new HttpClient();

        public enum EntityType
        {
            Foo = 0,
            AEntity = 1,
            BEntity = 2,
            CEntity = 3,
        }

        static void Main(string[] args)
        {


            client.BaseAddress = new Uri("http://cqbsapiquiz.azurewebsites.net/api/values/");


            RunAsync().GetAwaiter().GetResult();
        }


        static async Task RunAsync()
        {
            List<Foo> foos = await GetFooListAsync(FooEntityPath, true);
            List<AEntity> aEntities = await GetAListAsync(AEntityPath, true);
            List<BEntity> bEntities = await GetBListAsync(BEntityPath, true);


            Console.ForegroundColor = ConsoleColor.DarkGreen;
            foreach (Foo foo in foos)
            {
                Console.WriteLine(JsonConvert.SerializeObject(foo));
            }

            
            Console.ForegroundColor = ConsoleColor.DarkBlue;
            foreach (AEntity a in aEntities)
            {
              Console.WriteLine(JsonConvert.SerializeObject(a));
            }


            Console.ForegroundColor = ConsoleColor.DarkYellow;
            foreach (BEntity b in bEntities)
            {
                Console.WriteLine(JsonConvert.SerializeObject(b));
            }
            
            Console.ReadKey();
        }

        static async Task<List<Foo>> GetFooListAsync(string path, bool detailed = false)
        {
            string serilizedFooList = await GetRequestAsync(path);

            List<Foo> foos = new List<Foo>();

            if (!HasError(serilizedFooList))
            {
                foos = JsonConvert.DeserializeObject<List<Foo>>(serilizedFooList);

                if (detailed)
                {
                    foreach (Foo foo in foos)
                    {
                        string fooDetails = await GetRequestAsync(path + foo.Id.ToString());
                        Foo tmpFoo = JsonConvert.DeserializeObject<Foo>(fooDetails);
                        foo.ChildIds = tmpFoo.ChildIds;
                        foo.Details = tmpFoo.Details;
                        foo.IsClosed = tmpFoo.IsClosed;
                    }
                }
            }
          

            return foos;
        }

        static async Task<List<AEntity>> GetAListAsync(string path, bool detailed = false)
        {
            string serilizedAList = await GetRequestAsync(path);

            List<AEntity> aEntities = new List<AEntity>();

            if (!HasError(serilizedAList))
            {
                aEntities = JsonConvert.DeserializeObject<List<AEntity>>(serilizedAList);
                if (detailed)
                {
                    foreach (AEntity a in aEntities)
                    {
                        string ADetails = await GetRequestAsync(path + a.Id.ToString());
                        if(ADetails != null && !HasError(ADetails))
                        {
                            AEntity tmpAEntity = JsonConvert.DeserializeObject<AEntity>(ADetails);

                            a.Max = tmpAEntity.Max;
                            a.Min = tmpAEntity.Min;
                            a.CEntityId = tmpAEntity.CEntityId;
                            if(a.CEntityId != 0)
                            a.CEntityDetails = JsonConvert.DeserializeObject<CEntity>(await GetRequestAsync(a.CEntityId.ToString() + CEntityPath));
                        }

                    }
                }
            }
            return aEntities;
        }

        static async Task<List<BEntity>> GetBListAsync(string path, bool detailed = false)
        {
            string serilizedBList = await GetRequestAsync(path);

            List<BEntity> bEntities = new List<BEntity>();

            if (!HasError(serilizedBList)){
                bEntities = JsonConvert.DeserializeObject<List<BEntity>>(serilizedBList);

                if (detailed)
                {
                    foreach (BEntity b in bEntities)
                    {
                        string BDetails = await GetRequestAsync(path + b.Id.ToString());
                        BEntity tmpBEntity = JsonConvert.DeserializeObject<BEntity>(BDetails);

                        b.IsAwesome = tmpBEntity.IsAwesome;
                        b.IsTehSuck = tmpBEntity.IsTehSuck;
                        b.CEntityId = tmpBEntity.CEntityId;
                        if (b.CEntityId != 0)
                            b.CEntityDetails = JsonConvert.DeserializeObject<CEntity>(await GetRequestAsync(b.CEntityId.ToString() + CEntityPath));
                    }
                }
            }
            return bEntities;
        }

        static async Task<string> GetRequestAsync(string path)
        {
            string responseContent = "ErrorCode";
            int output = 0;
            while (HasError(responseContent))
            {

                HttpResponseMessage response = await client.GetAsync(path);
                if (response.IsSuccessStatusCode)
                {
                    responseContent = await response.Content.ReadAsStringAsync();
                }
                output++;
            }
            return responseContent;
        }

        static public bool HasError(string json)
        {
            if (json.Contains("ErrorCode"))
                return true;
            return false;
        }


       static void Log(string text)
        {
            Console.ForegroundColor = ConsoleColor.DarkRed;
            Console.WriteLine("DEBUG: " + text);
            Console.ForegroundColor = ConsoleColor.White;
        }
    }
}
