package com.sanevich

import com.sanevich.DoubleGenerator.{generateCards, needMoreCards}
import com.sanevich.model.*
import munit.FunSuite

class CardsTest extends FunSuite:
  test("generate 4 cards") {
    assertEquals(
      generateCards(3),
      Map(
        CardId(1) -> Card(
          List(
            Picture(value = 1, link = Some(CardId(2))),
            Picture(value = 2, link = Some(CardId(3))),
            Picture(value = 3, link = Some(CardId(4)))
          )
        ),
        CardId(2) -> Card(
          List(
            Picture(value = 1, link = Some(CardId(1))),
            Picture(value = 4, link = Some(CardId(3))),
            Picture(value = 5, link = Some(CardId(4)))
          )
        ),
        CardId(3) -> Card(
          List(
            Picture(value = 2, link = Some(CardId(1))),
            Picture(value = 4, link = Some(CardId(2))),
            Picture(value = 6, link = Some(CardId(4)))
          )
        ),
        CardId(4) -> Card(
          List(
            Picture(value = 3, link = Some(CardId(1))),
            Picture(value = 5, link = Some(CardId(2))),
            Picture(value = 6, link = Some(CardId(3)))
          )
        )
      )
    )
  }
