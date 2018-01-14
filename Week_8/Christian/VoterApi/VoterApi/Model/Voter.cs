using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace VoterApi.Model
{
    public class Voter
    {
        public Voter(
            DateTime date,
            string fips,
            string country, 
            int democratActive,
            int republicanActive, 
            int libertarianActive,
            int noPartyActive, 
            int otherActive,
            int totalActive,
            int democratInactive,
            int republicanInactive,
            int libertarianInactive,
            int noPartyInactive,
            int otherInactive,
            int totalInactive,
            int grandTotal,
            string primaryLatDec,
            string primaryLongDec
            )
        {
            Date = date;
            FIPS = fips;
            County = country;
            DemocratActive = democratActive;
            RepublicanActive = republicanActive;
            LibertarianActive = libertarianActive;
            NoPartyActive = noPartyActive;
            OtherActive = otherActive;
            TotalActive = totalActive;
            DemocratInactive = democratInactive;
            RepublicanInactive = republicanInactive;
            LibertarianInactive = libertarianInactive;
            NoPartyInactive = noPartyInactive;
            OtherInactive = otherInactive;
            TotalInactive = totalInactive;
            GrandTotal = grandTotal;
            PrimaryLatDec = primaryLatDec;
            PrimaryLongDec = primaryLongDec;
            PrimaryCountyCoordinates = $"({primaryLatDec}, {primaryLongDec})";
        }

        public DateTime Date { get; set; }
        public string FIPS { get; set; }
        public string County { get; set; }
        public int DemocratActive { get; set; }
        public int RepublicanActive { get; set; }
        public int LibertarianActive { get; set; }
        public int NoPartyActive { get; set; }
        public int OtherActive { get; set; }
        public int TotalActive { get; set; }
        public int DemocratInactive { get; set; }
        public int RepublicanInactive { get; set; }
        public int LibertarianInactive { get; set; }
        public int NoPartyInactive { get; set; }
        public int OtherInactive { get; set; }
        public int TotalInactive { get; set; }
        public int GrandTotal { get; set; }
        public string PrimaryLatDec { get; set; }
        public string PrimaryLongDec { get; set; }
        public string PrimaryCountyCoordinates { get; set; }

    }


}
