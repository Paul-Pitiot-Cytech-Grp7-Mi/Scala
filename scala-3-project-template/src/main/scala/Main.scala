import scala.annotation.tailrec

@main def hello(): Unit =
  // Tests isLeap
  println(s"2024 bissextile ? ${isLeap(2024)}")  // Some(true)
  println(s"2023 bissextile ? ${isLeap(2023)}")  // Some(false)
  println(s"1900 bissextile ? ${isLeap(1900)}")  // Some(false)
  println(s"2000 bissextile ? ${isLeap(2000)}")  // Some(true)
  println(s"-5 bissextile ? ${isLeap(-5)}")      // None

  // Tests numberOfDays
  println(s"Février 2024 : ${numberOfDays(true, 2)}")   // Some(29)
  println(s"Février 2023 : ${numberOfDays(false, 2)}")  // Some(28)
  println(s"Janvier : ${numberOfDays(false, 1)}")       // Some(31)
  println(s"Mois 13 : ${numberOfDays(false, 13)}")      // None

  println(s"pi(10) = ${pi(10)}")
  println(s"pi(100) = ${pi(100)}")
  println(s"pi(1000) = ${pi(1000)}")
  println(s"pi(10000) = ${pi(10000)}")
  println(s"Vrai pi = ${math.Pi}")
  println(s"piMC(1000) = ${piMC(1000)}")
  println(s"piMC(10000) = ${piMC(10000)}")
  println(s"piMC(100000) = ${piMC(100000)}")
  println(s"piMC(1000000) = ${piMC(1000000)}")
  println(s"Vrai pi = ${math.Pi}")

  // Tests Exercice 3
  println("\n=== Exercice 3.a: squareRoot (Newton) ===")
  println(s"sqrt(2) = ${squareRoot(2, 1e-10)}")
  println(s"sqrt(9) = ${squareRoot(9, 1e-10)}")
  println(s"sqrt(25) = ${squareRoot(25, 1e-10)}")

  println("\n=== Exercice 3.b: guess générique ===")
  println(s"sqrt(2) via guess = ${squareRootWithGuess(2, 1e-10)}")
  println(s"π via Newton sur sin = ${piNewton(1e-10)}")
  println(s"Vrai π = ${math.Pi}")

  println("\n=== Exercice 3.c: fixedPoint ===")
  println(s"Point fixe de cos = ${fixedPoint(1e-10, math.cos)(1.0)}")
  println(s"sqrt(2) via fixedPoint_damping = ${squareRootFixedPoint(2, 1e-10)}")
  println(s"sqrt(9) via fixedPoint_damping = ${squareRootFixedPoint(9, 1e-10)}")

// ====================
// Exercice 1
// ====================

// a. Vérifie si une année est bissextile
def isLeap(year: Int): Option[Boolean] =
  if year < 0 then None
  else Some(year % 4 == 0 && (year % 100 != 0 || year % 400 == 0))

// b. Nombre de jours dans un mois
def numberOfDays(leap: Boolean, month: Int): Option[Int] =
  month match
    case 1 | 3 | 5 | 7 | 8 | 10 | 12 => Some(31)
    case 4 | 6 | 9 | 11              => Some(30)
    case 2                            => if leap then Some(29) else Some(28)
    case _                            => None

// c. La signature pourrait être améliorée en utilisant un type plus précis
//    pour le mois (1-12) au lieu de Int, par exemple un enum Month.
//    Cela éviterait d'avoir à retourner Option[Int].

// ====================
// Exercice 2
// ====================

def pi(n: Int): Double =
  @tailrec
  def somme(k: Int, acc: Double): Double =
    if k > n then acc
    else somme(k + 1, acc + 1.0 / (k.toDouble * k.toDouble))
  
  math.sqrt(6 * somme(1, 0.0))

def piMC(n: Int): Double =
  val rand = new util.Random()
  
  @tailrec
  def count(k: Int, acc: Int): Int =
    if k >= n then acc
    else
      val x = rand.nextDouble()
      val y = rand.nextDouble()
      val inside = if x*x + y*y <= 1 then 1 else 0
      count(k + 1, acc + inside)
  
  4.0 * count(0, 0) / n

// c. pi est une fonction pure : pour un même n, elle retourne toujours le même résultat.
//    piMC n'est PAS pure : elle utilise un générateur aléatoire, donc pour un même n,
//    elle peut retourner des résultats différents à chaque appel.

// ====================
// Exercice 3
// ====================

// a. squareRoot avec méthode de Newton
// Newton pour f(x) = x² - a : x_{n+1} = x_n - f(x_n)/f'(x_n) = (x_n + a/x_n) / 2
def squareRoot(a: Double, eps: Double): Double =
  @tailrec
  def improve(guess: Double): Double =
    val nextGuess = (guess + a / guess) / 2
    if math.abs(nextGuess * nextGuess - a) < eps then nextGuess
    else improve(nextGuess)
  
  improve(1.0) // x0 = 1

// b. Fonction guess générique
@tailrec
def guess[T](isGoodEnough: T => Boolean, improveGuess: T => T)(initialGuess: T): T =
  if isGoodEnough(initialGuess) then initialGuess
  else guess(isGoodEnough, improveGuess)(improveGuess(initialGuess))

// b.(i) squareRoot redéfini avec guess
def squareRootWithGuess(a: Double, eps: Double): Double =
  val isGoodEnough: Double => Boolean = x => math.abs(x * x - a) < eps
  val improveGuess: Double => Double = x => (x + a / x) / 2
  guess(isGoodEnough, improveGuess)(1.0)

// b.(ii) π comme racine de sin avec Newton
// Pour f(x) = sin(x), f'(x) = cos(x)
// Newton: x_{n+1} = x_n - sin(x_n) / cos(x_n)
def piNewton(eps: Double): Double =
  val isGoodEnough: Double => Boolean = x => math.abs(math.sin(x)) < eps
  val improveGuess: Double => Double = x => x - math.sin(x) / math.cos(x)
  guess(isGoodEnough, improveGuess)(3.0) // initialGuess proche de π

// c.(i) fixedPoint - cherche x tel que f(x) = x
def fixedPoint(gap: Double, f: Double => Double)(initialGuess: Double): Double =
  @tailrec
  def iterate(current: Double): Double =
    val next = f(current)
    if math.abs(next - current) < gap then next
    else iterate(next)
  
  iterate(initialGuess)

// c.(ii) damping - fonction amortie: (x + f(x)) / 2
// Mathématiquement: si x est point fixe de f (f(x) = x), alors
//   damping(f)(x) = (x + f(x)) / 2 = (x + x) /