import Datos._
import Itinerarios._
import ItinerariosPar._

// ===== FUNCIONES QUE VAMOS A PROBAR =====
val its          = itinerarios(vuelosCurso, aeropuertosCurso)
val itsPar       = itinerariosPar(vuelosCurso, aeropuertosCurso)

val itsTiempo    = itinerariosTiempo(vuelosCurso, aeropuertosCurso)
val itsTiempoPar = itinerariosTiempoPar(vuelosCurso, aeropuertosCurso)

// Mapa GMT para calcular tiempos totales
val gmtMap: Map[String, Int] =
  aeropuertosCurso.map(a => a.Cod -> a.GMT).toMap

// Helper para imprimir
def showItinerario(it: Itinerario): String =
  it.map(v => s"${v.Org}->${v.Dst}").mkString(" / ")

println("\n===== SECCIÓN 2.1.1 — Encontrando itinerarios =====")

// Secuencial
val rSeq = its("CLO", "SVO")

println("\n>>> RESULTADOS SECUENCIALES (itinerarios)")
println(s"Total: ${rSeq.length}")
rSeq.zipWithIndex.foreach { case (it, i) =>
  println(s"  Seq ${i+1}: " + showItinerario(it))
}

// Paralelo
val rPar = itsPar("CLO", "SVO")

println("\n>>> RESULTADOS PARALELOS (itinerariosPar)")
println(s"Total: ${rPar.length}")
rPar.zipWithIndex.foreach { case (it, i) =>
  println(s"  Par ${i+1}: " + showItinerario(it))
}

// Comparación
println("\n>>> ¿SECUENCIAL == PARALELO?")
println(rSeq == rPar)  // true si funciona bien



// ============================
// 2.1.2 — Minimizar tiempo total
// ============================

println("\n===== SECCIÓN 2.1.2 — Minimizar tiempo total =====")

// Secuencial
val rSeqT = itsTiempo("CLO", "SVO")

println("\n>>> ITINERARIOS TIEMPO — SECUENCIAL")
rSeqT.zipWithIndex.foreach { case (it, i) =>
  val t = tiempoTotal(it, gmtMap)
  println(s"  Seq #${i+1} ($t min): " + showItinerario(it))
}

// Paralelo
val rParT = itsTiempoPar("CLO", "SVO")

println("\n>>> ITINERARIOS TIEMPO — PARALELO")
rParT.zipWithIndex.foreach { case (it, i) =>
  val t = tiempoTotal(it, gmtMap)
  println(s"  Par #${i+1} ($t min): " + showItinerario(it))
}
//Ojo: aca son 3 ya que se toman 3 para calcular el tiempo

// Comparación
println("\n>>> ¿SECUENCIAL == PARALELO? (tiempo mínimo)")
println(rSeqT == rParT)
