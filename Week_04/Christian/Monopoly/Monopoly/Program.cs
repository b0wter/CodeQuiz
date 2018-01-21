using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class Program
    {
        public static void Main(string[] args)
        {
            int numberOfDice = 2;
            int maxNumberOnDice = 4;
            int numberOfTurns = 200000;
         
            if (args.Length == 3)
            {
                int.TryParse(args[0], out numberOfDice);
                int.TryParse(args[1], out maxNumberOnDice);
                int.TryParse(args[2], out numberOfTurns);
            }

            Console.WriteLine(string.Format("Anzahl der Würfel: {0}, Augenzahl pro Würfel: {1}, Anzahl der Spielrunden: {2}", numberOfDice, maxNumberOnDice, numberOfTurns));
            
            Random r = new Random();
            DiceSet diceSet = InitializeDiceSet(numberOfDice, maxNumberOnDice, r); //Initialisieren der Würfel. Anzahl der Würfel, die Augenzahl pro Würfel und die (pseudo) Random Funktion. Diese wird nur einmal initialisiert, da ansonsten die Random Werte sich wiederholen können
            Player player = new Player(); //der Spieler. 
            Board board = new Board(diceSet, player, InitializeEreignissKarten(r), InitializeZufallsKarten(r)); //Initialisierung des Spielfeldes
            board.Play(numberOfTurns); //Start des Spieles

            //Ausgabe der Statistiken
            Console.WriteLine(board.GetThreeMostVisitedFields());
            Console.WriteLine(board.GetProbability());
            Console.ReadLine();
        }


        public static DiceSet InitializeDiceSet(int numberOfDice, int maxNumberOnDice, Random r)
        {
            
            List<Dice> diceList = new List<Dice>();
            for (int x = 0; x < numberOfDice; x++)
            {
                Dice dice = new Dice(maxNumberOnDice, r);
                diceList.Add(dice);
            }

            return new DiceSet(diceList.ToArray());
        }

        public static CardSet<EreignissKarte> InitializeEreignissKarten(Random r)
        {
            List<EreignissKarte> ereignissKarten = new List<EreignissKarte>();
            for (int x=0; x<14; x++)
            {
                ereignissKarten.Add(new EreignissKarte() { GotoField = Fields.None, Counter = x + 1 });
            }
    
            ereignissKarten.Add(new EreignissKarte() { GotoField = Fields.JAIL, Counter = 15 });
            ereignissKarten.Add(new EreignissKarte() { GotoField = Fields.GO, Counter = 16 });

            return new CardSet<EreignissKarte>(ereignissKarten, r);

        }

        public static CardSet<Zufallskarte> InitializeZufallsKarten(Random r)
        {
            List<Zufallskarte> zufallsKarten = new List<Zufallskarte>();
            for (int x = 0; x < 6; x++)
            {
                zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.None, SpecialAction = SpecialAction.None, Counter = x+1 });
            }

            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.JAIL, SpecialAction = SpecialAction.None, Counter = 7 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.GO, SpecialAction = SpecialAction.None, Counter = 8 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.C1, SpecialAction = SpecialAction.None, Counter = 9 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.E3, SpecialAction = SpecialAction.None, Counter = 10 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.H2, SpecialAction = SpecialAction.None, Counter = 11 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.R1, SpecialAction = SpecialAction.None, Counter = 12 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.None, SpecialAction = SpecialAction.MoveToNextR, Counter = 13 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.None, SpecialAction = SpecialAction.MoveToNextR, Counter = 14 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.None, SpecialAction = SpecialAction.MoveToNextU, Counter = 15 });
            zufallsKarten.Add(new Zufallskarte() { GotoField = Fields.None, SpecialAction = SpecialAction.MoveThreeFieldsBack, Counter = 16 });

            return new CardSet<Zufallskarte>(zufallsKarten, r);

        }

    }

    public enum Fields
    {
        GO = 0,
        A1 = 1,
        CC1 = 2,
        A2 = 3,
        T1 = 4,
        R1 = 5,
        B1 = 6,
        CH1 = 7,
        B2 = 8,
        B3 = 9,
        JAIL = 10,
        C1 = 11,
        U1 = 12,
        C2 = 13,
        C3 = 14,
        R2 = 15,
        D1 = 16,
        CC2 = 17,
        D2 = 18,
        D3 = 19,
        FP = 20,
        E1 = 21,
        CH2 = 22,
        E2 = 23,
        E3 = 24,
        R3 = 25,
        F1 = 26,
        F2 = 27,
        U2 = 28,
        F3 = 29,
        G2J = 30,
        G1 = 31,
        G2 = 32,
        CC3 = 33,
        G3 = 34,
        R4 = 35,
        CH3 = 36,
        H1 = 37,
        T2 = 38,
        H2 = 39,
        None = 100
    }
}
