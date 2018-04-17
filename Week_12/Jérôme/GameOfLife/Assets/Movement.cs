using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Movement : MonoBehaviour {

    [SerializeField]
    int movementSpeed = 5;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () {
        if (Input.GetKey(KeyCode.UpArrow))
            transform.position += Vector3.forward * Time.deltaTime * movementSpeed;

        if (Input.GetKey(KeyCode.DownArrow))
            transform.position -= Vector3.forward * Time.deltaTime * movementSpeed;

        if (Input.GetKey(KeyCode.W))
            transform.position +=Vector3.up * Time.deltaTime * movementSpeed;

        if (Input.GetKey(KeyCode.S))
            transform.position -= Vector3.up * Time.deltaTime * movementSpeed;
    }
}
