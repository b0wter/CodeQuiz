using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Dynamic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace WebApiServer.Data
{
    public class CsvDataProvider : IDataProvider
    {
        private const string filename = "data.csv";

        public readonly List<string> _header;
        public readonly List<List<string>> _data;

        public CsvDataProvider()
        {
            var records = GetDataFromCsv(filename);

            _data = records.Skip(1).ToList();
            _header = records[0].Select(x => x.ToLower()).ToList();
            foreach (var line in records.Skip(1))
                _data.Add(line.Select(x => x.ToLower()).ToList());
        }

        private List<List<string>> GetDataFromCsv(string filename)
        {
            var reader = new CsvHelper.CsvReader(new StringReader(File.ReadAllText(filename)));
            reader.Configuration.HasHeaderRecord = false;
            var records = new List<List<string>>();
            while(reader.Read())
            {
                var line = new List<string>();
                string value = string.Empty;
                for (int i = 0; reader.TryGetField<string>(i, out value); ++i)
                    line.Add(value);
                records.Add(line);
            }
            return records;
        }

        private IEnumerable<IEnumerable<string>> FilterByProperty(string property, string value)
        {
            property = property.ToLower();
            value = value.ToLower();
            if (_header.Contains(property) == false)
                throw new ArgumentException($"Die Property {property} ist unbekannt.");

            int index = _header.IndexOf(property);
            return _data.Where(x => x[index] == value);
        }

        public JArray FilterByProperties(Dictionary<string, string> filters)
        {
            IEnumerable<IEnumerable<string>> remaining = _data;
            foreach (var filter in filters)
                remaining = FilterByProperty(filter.Key, filter.Value);
            return ListsToJson(remaining);
        }

        public JArray ListsToJson(IEnumerable<IEnumerable<string>> lists)
        {
            var result = new JArray();
            foreach(var list in lists)
            {
                var jobject = new JObject();
                for (int i = 0; i < list.Count(); ++i)
                    jobject.Add(_header[i], JToken.FromObject(list.ElementAt(i)));
                result.Add(jobject);
            }
            return result;
        }
    }
}
