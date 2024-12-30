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

  private val picIdHolder: AtomicInteger = new AtomicInteger(1)

  /** Generate cards content based on desired number of pictures on one card
    * @param n - number of pictures on one card
    * @return cards
    */
  def generateCards(n: Int): Map[CardId, Card] = {
    val firstCard = Card(
      List.fill(n)(picIdHolder.getAndIncrement()).map(Picture(_, None))
    )

    @tailrec
    def loop(curList: Map[CardId, Card]): Map[CardId, Card] = {
      val newCardId = getAndIncrementCardId()
      val picList = (1 to n)
        .foldLeft(List[Picture]()) { case (curCard, _) =>
          @tailrec
          def choosePic(startFrom: Int): Picture = {
            val foundCards = curList.findByPicValue(startFrom)
            if (foundCards.size == 2) {
              choosePic(startFrom + 1)
            } else if (
              foundCards.size == 1 && !curCard.exists(
                _.value == startFrom
              ) && !foundCards.head._2.containsLinkOnCard(newCardId)
            ) {
              foundCards.head._2.content
                .find(_.value == startFrom)
                .foreach(_.link = Some(newCardId))
              Picture(startFrom, Some(foundCards.head._1))
            } else if (
              !curCard.exists(_.value == startFrom) && foundCards.isEmpty
            ) {
              Picture(startFrom, None)
            } else {
              choosePic(startFrom + 1)
            }
          }
          curCard :+ choosePic(1)
        }

      val newCard = Card(picList)

      val newCardsMap: Map[CardId, Card] = curList ++ Map(newCardId -> newCard)

      if (newCardsMap.containsNullLinks) {
        loop(newCardsMap)
      } else {
        newCardsMap
      }
    }

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
