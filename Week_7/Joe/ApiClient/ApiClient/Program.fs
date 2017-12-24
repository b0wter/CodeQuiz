open System
open FSharp.Data
open Newtonsoft.Json
open Newtonsoft.Json.FSharp

type EntityTypes = A | B | C | Foo
type Endpoints = Id of int64 | All

type CEntity   = { id:int64; description:string; hint:string }
type AEntity   = { id:int64; name:string; max:int; min:int; cEntityId:int64; details:Option<CEntity> }
type BEntity   = { id:int64; isAwesome:bool; isTehSuck:bool; cEntityId:int64; details:Option<CEntity> }
type FooEntity = { id:int64; name:string; isClosed:bool; childIds:int64 list; details:Option<CEntity>; a:Option<AEntity list>; b:Option<BEntity list> }

///**Description**
/// Basis-URL für alle Anfragen.
///
let baseUrl = "http://cqbsapiquiz.azurewebsites.net/api/values/"

///**Description**
/// Einfache Logging-Funktion zum Debuggen.
///
let log s = Console.WriteLine(s.ToString())

///**Description**
/// Zähler der eingehenden Datenmenge, fürs Debugging.
///
let mutable incomingDataCounter = 0

///**Description**
/// Standardeinstellungen für den Json-Serializer.
///
let serializerSettings =
    let settings = JsonSerializerSettings()
    settings.MissingMemberHandling <- MissingMemberHandling.Ignore
    settings.Converters.Add(OptionConverter ())
    settings

///**Description**
/// Wrapper um nicht-native (F#) Methode.
///
let jsonConvert<'a> (content:Option<string>) =
    match content with
    | None -> 
        failwith "Cannot deserialize none."
    | Some content -> 
        JsonConvert.DeserializeObject<'a>(content, serializerSettings)

///**Description**
/// Liefert für jeden EntityType den Subpath der Url zurück.
///
let entityTypesToUrlPart (t:EntityTypes) =
    match t with
    | A   -> "AEntity/"
    | B   -> "BEntity/"
    | C   -> "CEntity/"
    | Foo -> "Foo/"

///**Description**
/// Erzeugt aus einem Endpoint eine Textrepräsentation des Subpath der Url.
///
let endpointsToUrlPart (e:Endpoints) =
    match e with
    | All -> ""
    | Id id -> id.ToString() + "/"

///**Description**
/// Lädt einen String von einer Url herunter und führt zwei simple Tests durch. Schlagen diese fehl 
/// wird erneut versucht den Inhalt herunterzuladen.
///
let fromUrl (retries:uint32) url =
    //log ("Downloading from " + url)
    let rec retryIfEmpty (currentTry:uint32) : Option<string> =
        match currentTry with
        | _ when currentTry = retries -> 
            None
        | _ -> 
            let content = Http.RequestString (url, silentHttpErrors=true)
            match content with
            | "" -> 
                retryIfEmpty (currentTry + 1u)
            | _ when content.StartsWith("{\"Description\":\"Es ist ein interner Datenbankfehler aufgetreten, bitte erneut versuchen.\",\"ErrorCode\":-1}") ||
                     content.StartsWith("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">") ->
                log "Received error response, will try again."
                retryIfEmpty (currentTry + 1u)
            | _  -> 
                incomingDataCounter <- incomingDataCounter + content.Length
                Some content

    retryIfEmpty 0u

///**Description**
/// Partial application von fromUrl mit Retry-Count 10.
///
let fromUrlWithDefaults = fromUrl 10u

///**Description**
/// Erzeugt aus EntityType und Endpoint den kompletten Subpath für einen Query.
///
let createEntityUrl (target:(EntityTypes*Endpoints)) =
    let createBaseEntityUrl (t:EntityTypes) (e:Endpoints) =
        match t with
        | C ->
            (endpointsToUrlPart e) + (entityTypesToUrlPart t)
        | _ ->
            (entityTypesToUrlPart t) + (endpointsToUrlPart e)

    match target with
    | (centity, all) when centity = C && all = All -> 
        failwith "Cannot query all centities."
    | (entity, endpoint) ->
        baseUrl + createBaseEntityUrl entity endpoint

///**Description**
/// Function composition für die Zusammenstellung einer Download-Url und dem Download.
///
let entitiesFromUrl = createEntityUrl >> fromUrlWithDefaults

///**Description**
/// Einfacher Konsolenlogger.
///
let writeLine s =
    Console.WriteLine (s.ToString())

[<EntryPoint>]
let main _ = 
    // Lädt zuerst die Übersicht über die A/B/Foos herunter.
    let rawA    = entitiesFromUrl (A  , All) |> jsonConvert<AEntity list> 
    let rawB    = entitiesFromUrl (B  , All) |> jsonConvert<BEntity list>
    let rawFoo  = entitiesFromUrl (Foo, All) |> jsonConvert<FooEntity list>

    // Mit Hilfe der Liste werden die Detailsinformationen heruntergeladen.
    let fullA   = rawA   |> List.map (fun a -> (A, Id a.id)) |> List.map entitiesFromUrl |> List.map jsonConvert<AEntity>
    let fullB   = rawB   |> List.map (fun b -> (B, Id b.id)) |> List.map entitiesFromUrl |> List.map jsonConvert<BEntity>
    let fullC   = 
        let cReferences = Set.union (fullA |> List.map (fun a -> a.cEntityId) |> Set.ofList) (fullB |> List.map (fun b -> b.cEntityId) |> Set.ofList) |> Set.toList
        cReferences |> List.map (fun id -> (C, Id id)) |> List.map entitiesFromUrl |> List.map jsonConvert<CEntity>
    let fullFoo = rawFoo |> List.map (fun foo -> (Foo, Id foo.id)) |> List.map entitiesFromUrl |> List.map jsonConvert<FooEntity>

    let mappedA   = fullA   |> List.map (fun a -> {a with details = (fullC |> List.tryFind (fun c -> c.id = a.cEntityId))})
    let mappedB   = fullA   |> List.map (fun a -> {a with details = (fullC |> List.tryFind (fun c -> c.id = a.cEntityId))})
    let mappedFoo = fullFoo |> List.map (fun f -> 
                let aChildren = f.childIds |> List.map (fun childId -> fullA |> List.tryFind (fun x -> childId = x.id)) |> List.choose id
                let bChildren = f.childIds |> List.map (fun childId -> fullB |> List.tryFind (fun x -> childId = x.id)) |> List.choose id
                { f with a = Some aChildren; b = Some bChildren } )

    0 // return an integer exit code
