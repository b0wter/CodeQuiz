using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace WebApiServer
{
    public class Coordinate
    {
        public double Longitude { get; set; }
        public double Latitude { get; set; }

        public Coordinate()
        {
            //
        }

        public Coordinate(double longitude, double latitude)
        {
            Longitude = longitude;
            Latitude = latitude;
        }

        public bool TryParse(string s, out Coordinate coordinate)
        {
            coordinate = new Coordinate();

            if (s.Contains(",") == false)
                return false;

            int brackets = 0;
            if (s.Contains("("))
                brackets++;
            if (s.Contains(")"))
                brackets--;
            if (brackets != 0)
                return false;

            var parts = s.Split(",");
            if (parts.Length != 2)
                return false;

            double latitude, longitude;
            if (double.TryParse(s, out latitude) && double.TryParse(s, out longitude))
                return true;
            else
                return false;
        }
    }
}
