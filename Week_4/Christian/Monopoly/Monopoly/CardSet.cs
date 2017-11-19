using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Monopoly
{
    public class CardSet<T> where T : Card
    {
        private IOrderedEnumerable<T> _cardSet;
        private int currentPosition;
        public CardSet(List<T> cards, Random r)
        {
            _cardSet = Shuffle(cards, r);
            currentPosition = 0;
        }

        public IOrderedEnumerable<T> Shuffle (List<T> setOfCards, Random r)
        {
            return setOfCards.OrderBy(item => r.Next());
        }

        public T GetNextCardFromSet()
        {
            T card = _cardSet.Skip(currentPosition).Take(1).First();
            currentPosition++;

            if (currentPosition == _cardSet.Count())
            {
                currentPosition = 0;
            }

            return card;
        }

    }
}
