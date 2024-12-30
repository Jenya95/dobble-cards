package com.sanevich

import com.sanevich.model.*

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.annotation.tailrec

object DoubleGenerator:
  private val cardIdHolder: AtomicReference[CardId] =
    new AtomicReference[CardId](CardId(1))

  private def getAndIncrementCardId() = {
    cardIdHolder.getAndUpdate(c => c.increment)
  }

  /** Generate cards content based on desired number of pictures on one card
    * @param n - number of pictures on one card
    * @return cards
    */
  def generateCards(n: Int): Map[CardId, Card] = {

    @tailrec
    def createPicture(startFrom: Int, currentListPic: List[Picture], idToCard: Map[CardId, Card], newCardId: CardId): Picture = {
      idToCard.findByPicValue(startFrom) match {
        case foundCards if foundCards.size == 2 =>
          createPicture(startFrom + 1, currentListPic, idToCard, newCardId)

        case foundCards if foundCards.size == 1 =>
          val (existingCardId, existingCard) = foundCards.head
          if (!currentListPic.exists(_.value == startFrom) && !existingCard.containsLinkOnCard(newCardId)) {
            existingCard.content
              .find(_.value == startFrom)
              .foreach(_.link = Some(newCardId))
            Picture(startFrom, Some(existingCardId))
          } else {
            createPicture(startFrom + 1, currentListPic, idToCard, newCardId)
          }

        case _ if !currentListPic.exists(_.value == startFrom) =>
          Picture(startFrom, None)

        case _ =>
          createPicture(startFrom + 1, currentListPic, idToCard, newCardId)
      }
    }

    @tailrec
    def loop(cards: Map[CardId, Card]): Map[CardId, Card] = {
      val newCardId = getAndIncrementCardId()

      val picList = (1 to n)
        .foldLeft(List[Picture]()) { (currentListPic, _) =>
          currentListPic :+ createPicture(1, currentListPic, cards, newCardId)
        }

      val updatedCards = cards + (newCardId -> Card(picList))

      if (updatedCards.containsNullLinks) {
        loop(updatedCards)
      } else {
        updatedCards
      }
    }

    val firstCard = Card((1 to n).map(Picture(_, None)).toList)
    loop(Map(getAndIncrementCardId() -> firstCard))
  }

  extension (cardMap: Map[CardId, Card]) {
    private def findByPicValue(value: Int): Map[CardId, Card] = {
      cardMap.filter { case (_, v) =>
        v.containsPicWithValue(value)
      }
    }

    private def containsNullLinks: Boolean = {
      cardMap.exists(_._2.containsNullLink)
    }
  }

  extension (cards: IterableOnce[Card])
    def needMoreCards: Boolean = {
      !cards.iterator.forall { card =>
        card.content.forall { picId =>
          cards.iterator
            .filter(_ != card)
            .exists(_.content.contains(picId))
        }
      }
    }
