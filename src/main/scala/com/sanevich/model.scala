package com.sanevich

object model {
  opaque type CardId = Int
  object CardId:
    def apply(d: Int): CardId = d
    given Ordering[CardId]    = Ordering[Int]
  extension (c: CardId) {
    def increment: CardId = c + 1
  }

  case class Picture(value: Int, var link: Option[CardId])

  case class Card(content: List[Picture]) {
    def containsLinkOnCard(cardId: CardId): Boolean =
      content.exists(_.link.contains(cardId))

    def containsNullLink: Boolean =
      content.exists(_.link.isEmpty)

    def containsPicWithValue(value: Int): Boolean = {
      content.map(_.value).contains(value)
    }

    override def toString: String =
      content
        .map(c => s"${c.value} (link on ${c.link})")
        .mkString("[", ", ", "]")
  }

  object Card {
    def apply(n: Int*): Card =
      new Card(n.map(Picture(_, None)).toList)
  }
}
