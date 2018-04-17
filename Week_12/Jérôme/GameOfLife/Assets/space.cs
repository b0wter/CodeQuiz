using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Space {

    public bool NewPopulated { get; set; }
    bool populated;
    public bool Populated { get { return populated; } set
        {
           if(value != populated)
            {
                populated = value;
                if (populated)
                    PopulatedCount++;
            }
            
        }
    }
    public List<Space> Neighbours { get; set; }
    public Vector3 Position { get; set; }

    int populatedCount;
    public int PopulatedCount
    {
        get { return populatedCount; } set
        {
            if (PopulatedCount < 10)
                populatedCount = value;
        }
    }
    public int Count { get; set; }
    public int PopulatedNeigbours { get {
            int count = 0;
            foreach(Space neighbour in Neighbours)
            {
                if(neighbour.Populated == true)
                {
                    count++;
                }
            }
            return count;
        }
    }


    public Space(Vector3 pos, int count)
    {
        NewPopulated = false;
        Populated = false;
        Neighbours = new List<Space>();
        Position = pos;
        Count = count;
        populatedCount = 1;
    }

    public Space()
    {

    }
}
