using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using VoterApi.Model;

namespace VoterApi.DataAccess
{
    public class VoterFileAccess
    {
        public IEnumerable<Voter> GetVoterListFromFile(string fileName)
        {
            List<Voter> voterList = new List<Voter>();

            string csvData = File.ReadAllText(fileName);
            bool isFirstLine = true;

            foreach (string row in csvData.Split('\n'))
            {
                string[] cells = row.Split(',');                
                if (cells != null && cells.Length > 0)
                {
                    if (!isFirstLine) //der Einfachheit halber ignorieren wir die Bezeichnungen der Spalten aus dem File und nutzen für der JSON die im Voter Objekt definierten
                    {
                        DateTime dateTime;
                        if (DateTime.TryParse(cells[0], out dateTime)) //hier könnte natürlich ein Fehler Handling erfolgen
                        {
                            voterList.Add(
                                new Voter(
                                dateTime,
                                cells[1],
                                cells[2],
                                string.IsNullOrEmpty(cells[3]) ? 0 : Convert.ToInt32(cells[3]),
                                string.IsNullOrEmpty(cells[4]) ? 0 : Convert.ToInt32(cells[4]),
                                string.IsNullOrEmpty(cells[5]) ? 0 : Convert.ToInt32(cells[5]),
                                string.IsNullOrEmpty(cells[6]) ? 0 : Convert.ToInt32(cells[6]),
                                string.IsNullOrEmpty(cells[7]) ? 0 : Convert.ToInt32(cells[7]),
                                string.IsNullOrEmpty(cells[8]) ? 0 : Convert.ToInt32(cells[8]),
                                string.IsNullOrEmpty(cells[9]) ? 0 : Convert.ToInt32(cells[9]),
                                string.IsNullOrEmpty(cells[10]) ? 0 : Convert.ToInt32(cells[10]),
                                string.IsNullOrEmpty(cells[11]) ? 0 : Convert.ToInt32(cells[11]),
                                string.IsNullOrEmpty(cells[12]) ? 0 : Convert.ToInt32(cells[12]),
                                string.IsNullOrEmpty(cells[13]) ? 0 : Convert.ToInt32(cells[13]),
                                string.IsNullOrEmpty(cells[14]) ? 0 : Convert.ToInt32(cells[14]),
                                string.IsNullOrEmpty(cells[15]) ? 0 : Convert.ToInt32(cells[15]),
                                cells[16],
                                cells[17]
                              )
                           );
                        }
                    }
                }

                isFirstLine = false;
            }

            return voterList;
        }
        
    }
}
