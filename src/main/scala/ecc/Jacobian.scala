package ecc

class Jacobian(val curve: Curve, _x: BigInt, _y: BigInt, _z: Option[BigInt]) extends Field[Jacobian] {
  override val order = curve.q
  val X = BigIntMod(_x, curve.q)
  val Y = BigIntMod(_y, curve.q)
  val Z = if (_z == None) curve.l else BigIntMod(_z.get, curve.q)
  val isIdentity = _y == null

  def this(p: CurvePoint) =
    this(p.curve, p.x, p.y, None)

  def affine: CurvePoint = {
    val zi = Z.inv
    val zi2 = Z.inv.pow(2)
    return curve.point(X * zi2, Y * zi2 * zi)
  }

  def x2: Jacobian = {
    val A = Y.pow(2)
    val B = X * A * 4
    val C = A.pow(2) * 8
    val D =
      if (curve.minusthree) {
        val ZZ = Z.pow(2)
        (X + ZZ) * (X - ZZ) * 3
      } else
        (X.pow(2) * 3) + (Z.pow(4) * curve.a)

    val X3 = D.pow(2) - B * 2
    return new Jacobian(
      curve,
      X3,
      D * (B - X3) - C,
      Some(Y * Z * 2))
  }

  def +(_that: Jacobian): Jacobian = {
    val that =
      if (_that.Z == 1)
        _that
      else {
        new Jacobian(_that.affine)
      }

    val A = Z.pow(2)
    val B = Z * A
    val C = that.X * A
    val D = that.Y * B
    val E = C - X
    val F = D - Y
    val G = E.pow(2)
    val H = G * E
    val I = X * G
    val X3 = F.pow(2) - (H + I * 2)
    return new Jacobian(
      curve,
      X3,
      F * (I - X3) - Y * H,
      Some(Z * E))
  }
}


