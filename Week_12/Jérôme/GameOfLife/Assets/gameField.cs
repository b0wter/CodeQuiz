using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Gamefield  {

    /// <summary>
    /// Public
    /// </summary>

    public int Size { get; set; }

    public bool PartyModus { get; set; }

    public int TotalPopulation { get; set; }

    /// <summary>
    /// Private
    /// </summary>

    List<List<Space>> Spaces { get; set; }

    List<GameObject> CubeList { get; set; }

    Gradient PartyGradient { get; set; }

    GameObject Cube { get; set; }

    Transform GameFieldTransform { get; set; }



    public Gamefield(Gradient gradient, GameObject cube, Transform gameFieldTransform)
    {
        Size = 50;
        PartyModus = false;
        TotalPopulation = 0;

        Spaces = new List<List<Space>>();
        CubeList = new List<GameObject>();
        Init();
        SetNeighbours();

        Cube = cube;
        PartyGradient = gradient;
        GameFieldTransform = gameFieldTransform;
    }


    /// <summary>
    /// Private Methods
    /// </summary>

    void Init()
    {
        for (int i = 0; i < Size; i++)
        {
            Spaces.Add(new List<Space>());
            for (int j = 0; j < Size; j++)
            {
                Spaces[i].Add(new Space(new Vector3(i, j), i * Size + j));
            }
        }
    }

    void SetNeighbours()
    {
        for (int i = 0; i < Size; i++)
        {
            for (int j = 0; j < Size; j++)
            {
                if (i > 0)
                {
                    Spaces[i][j].Neighbours.Add(Spaces[i - 1][j]);

                    if (j > 0)
                    {
                        Spaces[i][j].Neighbours.Add(Spaces[i - 1][j - 1]);
                    }
                }

                if (i < Size - 1)
                {
                    Spaces[i][j].Neighbours.Add(Spaces[i + 1][j]);

                    if (j < Size - 1)
                        Spaces[i][j].Neighbours.Add(Spaces[i + 1][j + 1]);

                }

                if (j > 0)
                {
                    Spaces[i][j].Neighbours.Add(Spaces[i][j - 1]);

                    if (i < Size - 1)
                        Spaces[i][j].Neighbours.Add(Spaces[i + 1][j - 1]);

                }

                if (j < Size - 1)
                {
                    Spaces[i][j].Neighbours.Add(Spaces[i][j + 1]);

                    if (i > 0)
                        Spaces[i][j].Neighbours.Add(Spaces[i - 1][j + 1]);

                }
            }
        }
    }

    void UpdatePopulation()
    {
        foreach (List<Space> spaceRow in Spaces)
        {
            foreach (Space space in spaceRow)
            {
                if (!space.Populated && space.PopulatedNeigbours == 3)
                    space.NewPopulated = true;
                else if (space.Populated && (space.PopulatedNeigbours == 3 || space.PopulatedNeigbours == 2))
                    space.NewPopulated = true;
                else
                    space.NewPopulated = false;
            }
        }
        SetUpdatedPopulation();
    }

    void UpdateGameField()
    {
        foreach (List<Space> spaceRow in Spaces)
        {
            foreach (Space space in spaceRow)
            {
                CubeList[space.Count].GetComponent<Renderer>().material.color = GetColorForState(space.Populated);
                Vector3 currentPos = CubeList[space.Count].transform.position;
                if (PartyModus && space.Populated)
                {
                    CubeList[space.Count].transform.localScale = new Vector3(1, 1, Random.Range(1.0f, 5.0f));
                }

                else
                {
                    CubeList[space.Count].transform.localScale = new Vector3(1, 1, 1);
                }
                CubeList[space.Count].transform.position = new Vector3(currentPos.x, currentPos.y, space.PopulatedCount);
            }
        }
    }

    void SetUpdatedPopulation()
    {
        int counter = 0;
        foreach (List<Space> spaceRow in Spaces)
        {
            foreach (Space space in spaceRow)
            {
                space.Populated = space.NewPopulated;

                if (space.Populated)
                    counter++;
            }
        }
        TotalPopulation = counter;
    }

    Color GetColorForState(bool state)
    {
        if (state && PartyModus)
            return PartyGradient.Evaluate(Random.Range(0.0f, 1.00f));
        else if (state && !PartyModus)
            return Color.green;
        else
            return Color.black;
    }

    void ClearLists()
    {
        foreach (GameObject cube in CubeList)
            GameObject.Destroy(cube);

        Spaces.Clear();
        CubeList.Clear();
    }


    /// <summary>
    /// Public Methods
    /// </summary>

    public void RandomPopulation(int count=-1)
    {
        if (count == -1)
            count = Size*10;

        for (int i = 0; i <= count; i++)
        {
            int x = (int)(Random.Range(0.0f, (Size - 1.0f) / 100.0f) * 100);
            int y = (int)(Random.Range(0.0f, (Size - 1.0f) / 100.0f) * 100);
            Spaces[x][y].Populated = true;
        }
    }

    public void CreateGameField()
    {
        int count = 1;
        foreach (List<Space> spaceRow in Spaces)
        {
            foreach (Space space in spaceRow)
            {
                GameObject go = GameObject.Instantiate(Cube, GameFieldTransform);
                go.transform.position = space.Position;
                go.GetComponent<Renderer>().material.color = GetColorForState(space.Populated);
                go.name = "cube" + count.ToString();
                count++;
                CubeList.Add(go);
            }
        }
    }

    public void Update()
    {
        UpdatePopulation();
        SetUpdatedPopulation();
        UpdateGameField();
    }

    public void UpdateCube(GameObject cubeToUpdate)
    {
        int index = CubeList.IndexOf(cubeToUpdate);
        int j = index % Size;
        int i = (index - j) / Size;

        Spaces[i][j].Populated = true;

        CubeList[index].GetComponent<Renderer>().material.color = GetColorForState(true);
    }

    public void Restart()
    {
        ClearLists();

        Init();
        SetNeighbours();
        RandomPopulation();
        CreateGameField();
    }
}
