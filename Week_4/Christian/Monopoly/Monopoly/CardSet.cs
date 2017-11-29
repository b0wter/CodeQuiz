using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class CardSet<T> where T : Card
    {
        private List<T> _cardSet;
        private int currentPosition;
        public CardSet(List<T> cards, Random r)
        {
            _cardSet = Shuffle(cards, r);
            currentPosition = 0;
        }

        public List<T> Shuffle (List<T> setOfCards, Random r)
        {
            return setOfCards.OrderBy(item => r.Next()).ToList();
        }

        public T GetNextCardFromSet()
        {
            T card = _cardSet.ElementAt(currentPosition);
            currentPosition++;

            if (currentPosition == _cardSet.Count())
            {
                currentPosition = 0;
            }

            return card;
        }

    }
}
