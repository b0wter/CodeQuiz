using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class Board
    {
        private DiceSet _diceSet;
        private Player _player;
        private Dictionary<Fields, int> recorder;
        private CardSet<EreignissKarte> _ereignissKarten;
        private CardSet<Zufallskarte> _zufallsKarten;
        private int _numberOfTurns;

        public Board(
            DiceSet diceSet,
            Player player,
            CardSet<EreignissKarte> ereignissKarten,
            CardSet<Zufallskarte> zufallsKarten)
        {
            recorder = new Dictionary<Fields, int>();

            _diceSet = diceSet;
            _player = player;
            _zufallsKarten = zufallsKarten;
            _ereignissKarten = ereignissKarten;
        }

        public void Play(int numberOfTurns)
        {
            _numberOfTurns = numberOfTurns;

            for (int x = 0; x < numberOfTurns; x++)
            {
                ExecuteTurn(0);
            }
        }


        public void ExecuteTurn(int sameOfAKind)
        {
            DiceSetResult diceRes = _player.RollDice(_diceSet);

            if (sameOfAKind == 2 && diceRes.XOfAKind) //3 x Pasch -> ab ins Gefängniss
            {
                _player.SetPosition(Fields.JAIL);
                RecordPosition(); //Erfasse Endposition
                return;
            }

            MovePlayer(diceRes.Result); //Bewegen
            CheckForSpecialEvents(); //Ereignisfeld / Zufallskarten Feld?

            RecordPosition(); //Erfasse Endposition

            if (diceRes.XOfAKind)
            {
                ExecuteTurn(sameOfAKind+1);
            }
        }

        private List<Fields> OrderByMostProbable()
        {
            return recorder.OrderByDescending(val => val.Value).Select(key => key.Key).ToList();
        }
        public string GetThreeMostVisitedFields()
        {
            List<Fields> fields = OrderByMostProbable();
            return string.Format("Die drei meistbesuchten Felder sind: {0}{1}{2}", fields.First(), fields.Skip(1).Take(1).First(), fields.Skip(2).Take(1).First());
        }

        public string GetProbability()
        {   
            int total = recorder.Sum(val => val.Value);

            List<Fields> fields = OrderByMostProbable();
            
            decimal firstField = recorder[fields.First()];
            decimal secondField = recorder[fields.Skip(1).Take(1).First()];
            decimal thirdfield = recorder[fields.Skip(2).Take(1).First()];

            decimal firstProbability = firstField / total * 100;
            decimal secondProbability = secondField / total * 100;
            decimal thirdProbability = thirdfield / total * 100;    

            return string.Format("Wahrscheinlichkeiten: {0:0.00} : {1:0.00} : {2:0.00}", firstProbability, secondProbability, thirdProbability);
        }

        public void MovePlayer(int diceRoll)
        {
            int position = (int)_player.GetPosition();
            position += diceRoll;

            if (position > 39)
            {
                position = Math.Abs(position - 40);
            }

            Fields newPosition = (Fields)position;

            _player.SetPosition(newPosition);
        }

        public void CheckForSpecialEvents()
        {
            if (_player.GetPosition() == Fields.G2J)
            {
                _player.SetPosition(Fields.JAIL); //Ha ha!
                return;
            }

            if (_player.GetPosition() == Fields.CC1 ||
                _player.GetPosition() == Fields.CC2 ||
                _player.GetPosition() == Fields.CC3)
            {
                ZieheGemeinschaftskarte();
                return;
            }

            if (_player.GetPosition() == Fields.CH1 ||
                _player.GetPosition() == Fields.CH2 ||
                _player.GetPosition() == Fields.CH3)
            {
                ZieheZufallskarte();
                return;
            }
        }

        public void ZieheGemeinschaftskarte()
        {
            EreignissKarte ereignissKarte = _ereignissKarten.GetNextCardFromSet();
            if (ereignissKarte.GotoField != Fields.None)
            {
                _player.SetPosition(ereignissKarte.GotoField);
            }           
        }

        public void ZieheZufallskarte()
        {           
            Zufallskarte zufallsKarte = _zufallsKarten.GetNextCardFromSet();
            if (zufallsKarte.GotoField != Fields.None)
            {
                _player.SetPosition(zufallsKarte.GotoField);
                return;
            }

            if (zufallsKarte.SpecialAction == SpecialAction.MoveThreeFieldsBack)
            {
                if ((int)_player.GetPosition() > 2)
                {
                    _player.SetPosition((Fields)_player.GetPosition() - 3);
                }
                else
                {
                    _player.SetPosition((Fields)_player.GetPosition() + 40 - 3);
                }

                return;
            }

            if (zufallsKarte.SpecialAction == SpecialAction.MoveToNextR)
            {
                if ((int)_player.GetPosition() < 5)
                {
                    _player.SetPosition(Fields.R1);
                }
                else if ((int)_player.GetPosition() < 15)
                {
                    _player.SetPosition(Fields.R2);
                }
                else if ((int)_player.GetPosition() < 25)
                {
                    _player.SetPosition(Fields.R3);
                }
                else if ((int)_player.GetPosition() < 35)
                {
                    _player.SetPosition(Fields.R4);
                }

                return;
            }

            if (zufallsKarte.SpecialAction == SpecialAction.MoveToNextU)
            {
                if ((int)_player.GetPosition() < 12)
                {
                    _player.SetPosition(Fields.U1);
                }
                else if ((int)_player.GetPosition() < 28)
                {
                    _player.SetPosition(Fields.U2);
                }                

                return;
            }

        }
        public void RecordPosition()
        {
            if (recorder.ContainsKey(_player.GetPosition()))
            {
                recorder[_player.GetPosition()]++;
            }
            else
            {
                recorder.Add(_player.GetPosition(), 1);
            }
        }
    }
}
