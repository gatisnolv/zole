package com.gatis.bootcamp.project.zole

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CardSpec extends AnyFlatSpec with Matchers {

  it should "be corrrect" in {
    Card.of("7h") shouldBe a[Left[_, _]]
    Card.of("7c") shouldBe a[Left[_, _]]
    Card.of("7s") shouldBe a[Left[_, _]]
    Card.of("8h") shouldBe a[Left[_, _]]
    Card.of("8c") shouldBe a[Left[_, _]]
    Card.of("8s") shouldBe a[Left[_, _]]
    Card.of("7d") shouldBe a[Right[_, _]]
    Card.of("8d") shouldBe a[Right[_, _]]
  }

}
