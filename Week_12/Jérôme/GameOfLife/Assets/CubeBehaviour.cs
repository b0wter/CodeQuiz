using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CubeBehaviour : MonoBehaviour {


    GameObject main;

	// Use this for initialization
	void Start () {
        main = GameObject.FindGameObjectWithTag("MainObject");
	}
	
	// Update is called once per frame
	void Update () {
		
	}

    private void OnMouseOver()
    {
        if (Input.GetMouseButtonDown(0))
            main.GetComponent<gamelogic>().ManuallyPopulated(gameObject);
       
    }
}
