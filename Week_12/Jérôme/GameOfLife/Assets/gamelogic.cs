using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class gamelogic : MonoBehaviour
{

    [SerializeField]
    int rowAndColumnSize = 100;

    [SerializeField]
    Gradient partyGradient;

    [SerializeField]
    List<Light> partyLights;

    [SerializeField]
    GameObject cube;

    [SerializeField]
    GameObject gameFieldObject;

    [SerializeField]
    Text text;

    [SerializeField]
    Text cycleText;

    [SerializeField]
    List<GameObject> fireWorkList;

    [SerializeField]
    float cyleTime = 0.1f;

    AudioSource audioSource;

    bool pause = true;

    bool partyModus = false;

    float tmpTime = 0.0f;

    int cycleCount = 0;

    Coroutine partyRoutine;

    Button pauseButton;

    Gamefield gamefield;
    // Use this for initialization
    void Start()
    {
        gamefield = new Gamefield(partyGradient, cube, gameFieldObject.transform);

        gamefield.RandomPopulation();
        gamefield.CreateGameField();

        audioSource = GetComponent<AudioSource>();
    }

    // Update is called once per frame
    void Update()
    {

        if (!pause)
        {
            tmpTime += Time.deltaTime;
            if (tmpTime > cyleTime)
            {
                tmpTime = 0;
                cycleCount++;
                cycleText.text = cycleCount.ToString();
                gamefield.Update();
                text.text = gamefield.TotalPopulation.ToString();
            }
        }

        if (Input.GetKeyDown(KeyCode.P))
        {
            if (!audioSource.isPlaying)
                audioSource.Play();

            partyRoutine = StartCoroutine(ActivatePartyModus());

            GameObject.FindGameObjectWithTag("MainCamera").GetComponent<Camera>().backgroundColor = Color.black;
        }

        if (Input.GetKeyDown(KeyCode.M))
        {
            if (audioSource.isPlaying)
                audioSource.Stop();
            gamefield.PartyModus = false;

            if (partyRoutine != null)
                StopCoroutine(partyRoutine);

            foreach (Light light in partyLights)
                light.enabled = false;


            foreach (GameObject fw in fireWorkList)
                fw.SetActive(false);

            GameObject.FindGameObjectWithTag("MainCamera").GetComponent<Camera>().backgroundColor = Color.white;

        }

        if (Input.GetKeyDown(KeyCode.Q))
            Application.Quit();

        if (gamefield.PartyModus)
        {
            foreach (Light light in partyLights)
            {
                light.color = partyGradient.Evaluate(Random.Range(0.0f, 1.00f));
            }
        }
    }

    public void ManuallyPopulated(GameObject sender)
    {
        gamefield.UpdateCube(sender);
    }

    IEnumerator ActivatePartyModus()
    { 
        yield return new WaitForSeconds(12);

        gamefield.PartyModus = true;
        foreach (Light light in partyLights)
            light.enabled = true;


        foreach (GameObject fw in fireWorkList)
            fw.SetActive(true);

    }

    public void SetCycleTime(Slider slider)
    {
        this.cyleTime = slider.value;
        slider.GetComponentInChildren<Text>().text = slider.value.ToString() + " s";

    }

    public void onButtonClick(Button button)
    {
        if (button.name == "RestartButton")
        {
            bool safePauseState = pause;
            pause = true;
            gamefield.Restart();
            pause = safePauseState;
            cycleCount = 0;
        }
        else if (button.name == "PauseButton")
        {
            pause = !pause;
            Text text = button.GetComponentInChildren<Text>();

            if (pause)
                text.text = "Unpause";
            else
                text.text = "Pause";

        }

    }

}


