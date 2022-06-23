package Arithm

final case class MathException(val message: String, val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class ExpressionException(val message: String, val cause: Throwable = None.orNull)
  extends Exception(message, cause)
