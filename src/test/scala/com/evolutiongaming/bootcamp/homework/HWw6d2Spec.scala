package com.evolutiongaming.bootcamp.error_handling

import cats.syntax.all._
import com.evolutiongaming.bootcamp.error_handling.PaymentCardHW._
import org.scalacheck.Gen._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.YearMonth

class ErrorHandlingHWSpec
  extends AnyFlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  "PaymentCard" should "handle valid and invalid cards" in {
    import ValidationError._

    PaymentCard(
      name = "Kirill Kartinkin",
      number = "4321123443211234",
      expirationDate = "10/2020",
      securityCode = "666"
    ) shouldBe
      PaymentCard(
        OwnerName("Kirill Kartinkin"),
        CardNumber("4321123443211234"),
        ExpirationDate(YearMonth.of(2020, 10)),
        CVVNumber("666"))
      .validNec

    def checkInvalid(
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String,
      errors: Set[ValidationError]
    ): Assertion =
      PaymentCard(name, number, expirationDate, securityCode)
      .leftMap(_.toList.toSet) shouldBe errors.invalid

    checkInvalid(
      name = "Ki",
      number = "4321123443211234",
      expirationDate = "10/2020",
      securityCode = "666",
      errors = Set(OwnernameIsInvalid),
    )

    checkInvalid(
      name = "Kirill Kartinkin",
      number = "5321123443211234",
      expirationDate = "10/2020",
      securityCode = "666",
      errors = Set(UnknownIssuer, CardNumberIsInvalid),
    )

    checkInvalid(
      name = "Kirill Kartinkin",
      number = "43",
      expirationDate = "10/2020",
      securityCode = "666",
      errors = Set(CardNumberIsInvalid),
    )

    checkInvalid(
      name = "Kirill Kartinkin",
      number = "4321123443211234",
      expirationDate = "15/2020",
      securityCode = "666",
      errors = Set(ExpirationDateIsInvalid),
    )

    checkInvalid(
      name = "Kirill Kartinkin",
      number = "4321123443211234",
      expirationDate = "10/2020",
      securityCode = "",
      errors = Set(SecurityCodeIsInvalid),
    )

    checkInvalid(
      name = "",
      number = "",
      expirationDate = "",
      securityCode = "",
      errors = Set(
        OwnernameIsInvalid,
        UnknownIssuer,
        CardNumberIsInvalid,
        ExpirationDateIsInvalid,
        SecurityCodeIsInvalid),
    )
  }
}
