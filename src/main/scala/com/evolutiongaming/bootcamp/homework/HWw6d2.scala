package com.evolutiongaming.bootcamp.error_handling

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.Try

import cats.data.Validated
import cats.data.ValidatedNec
import cats.syntax.all._

import java.time.format.DateTimeFormatter
import java.time.YearMonth

object PaymentCardHW {

  final case class OwnerName(v: String) extends AnyVal
  final case class CardNumber(v: String) extends AnyVal
  final case class ExpirationDate(v: YearMonth) extends AnyVal
  final case class CVVNumber(v: String) extends AnyVal

  final case class PaymentCard(
    owner: OwnerName,
    cardNumber: CardNumber,
    expirationDate: ExpirationDate,
    cvvNumber: CVVNumber
  )

  sealed trait ValidationError
  object ValidationError {
    final case object OwnernameIsInvalid extends ValidationError {
      override def toString: String = "Owner's name must be between 3 and 30 alphabetic characters"
    }
    final case object UnknownIssuer extends ValidationError {
      override def toString: String = "Only VISA cards are supported"
    }
    final case object CardNumberIsInvalid extends ValidationError {
      override def toString: String = "Card number should be exactly 16 digits long"
    }
    final case object CardNumberHasWrongChecksum extends ValidationError {
      override def toString: String = "Invalid card number"
    }
    final case object ExpirationDateIsInvalid extends ValidationError {
      override def toString: String = "Expiration date should be in MM/yyyy format"
    }
    final case object SecurityCodeIsInvalid extends ValidationError {
      override def toString: String = "CVV number shoiuld be exactly 3 digits long"
    }
  }

  object PaymentCardValidator {
    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateOwnerName(name: String): AllErrorsOr[OwnerName] = {
      def validateNameRegEx: AllErrorsOr[OwnerName] =
        Validated.condNec(name.matches("^[a-zA-Z ]{3,30}$"), OwnerName(name), OwnernameIsInvalid)

      validateNameRegEx
    }

    private def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {
      def validateCardIssuer: AllErrorsOr[CardNumber] =
        Validated.condNec(number.matches("^4.*"), CardNumber(number), UnknownIssuer)

      def validateCardNumberRegEx: AllErrorsOr[CardNumber] =
        Validated.condNec(number.matches("^4[0-9]{12}(?:[0-9]{3})?$"), CardNumber(number), CardNumberIsInvalid)

      def validateCardNumberChecksum: AllErrorsOr[CardNumber] =
        Validated.condNec(true, CardNumber(number), CardNumberHasWrongChecksum)

      validateCardIssuer productR validateCardNumberRegEx productR validateCardNumberChecksum
    }

    private def validateExpirationDate(date: String): AllErrorsOr[ExpirationDate] = {
      def validateYearMonthParser: AllErrorsOr[ExpirationDate] =
        Try(YearMonth.parse(date, DateTimeFormatter.ofPattern("MM/yyyy")))
        .toOption
        .toValidNec(ExpirationDateIsInvalid)
        .map(ExpirationDate)

      validateYearMonthParser
    }

    private def validateSecurityCode(securityCode: String): AllErrorsOr[CVVNumber] = {
      def validateSecurityCodeRegEx: AllErrorsOr[CVVNumber] =
        Validated.condNec(securityCode.matches("^[0-9]{3}$"), CVVNumber(securityCode), SecurityCodeIsInvalid)

      validateSecurityCodeRegEx
    }

    def validate(
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String,
    ): AllErrorsOr[PaymentCard] =
    (
      validateOwnerName(name),
      validateCardNumber(number),
      validateExpirationDate(expirationDate),
      validateSecurityCode(securityCode)
    ).mapN(PaymentCard)
  }
}
