import Datos._
import Itinerarios._
import ItinerariosPar._


vuelosCurso.take(2)  // Prueba para comprobar que sí hay funcionamiento.


//Pruebas para 3.1 ENUNCIADO (Corresponde a 2.1.1)
//Estas son las pruebas que el enunciado plantea para itinerarios, Devuelven
//Exactamente lo que pide el enunciado por tanto son correctas (eso espero xd)
val itsCurso = itinerarios(vuelosCurso, aeropuertosCurso)

//Prueba itinerariosTiempo
//2.1 Aeropuertos Incomunicados
val its1 = itsCurso("MID", "SVCS")
val its2 = itsCurso("CLO", "SVCS")

//4 Itinerarios CLO-SVO
val its3 = itsCurso("CLO", "SVO")

//2 Itinerarios CLO-MEX
val its4 = itsCurso("CLO", "MEX")

//2 Itinerarios CTG-PTY
val its5 = itsCurso("CTG", "PTY")


//Pruebas para 3.2 ENUNCIADO (Correspondiente a 2.1.2)
//Devuelven exactamente lo que pide el enunciado por tanto son correctas (eso espero xd)
val itsTiempoCurso = itinerariosTiempo(vuelosCurso, aeropuertosCurso)
//Prueba itinerariosTiempo
val itst1 = itsTiempoCurso("MID", "SVCS")
val itst2 = itsTiempoCurso("CLO", "SVCS")

//4 Itinerarios CLO-SVO
val itst3 = itsTiempoCurso("CLO", "SVO")

//2 Itinerarios CLO-MEX
val itst4 = itsTiempoCurso("CLO", "MEX")

//2 Itinerarios CTG-PTY
val itst5 = itsTiempoCurso("CTG", "PTY")


//Pruebas para 3.3 ENUNCIADO (Correspondiente a 2.1.3)
//Devuelven exactamente lo que pide el enunciado por tanto son correctas (eso espero xd)
val itsEscalasCurso = itinerariosEscalas(vuelosCurso, aeropuertosCurso)
//Pruebas itinerariosEscalas
val itsc1 = itsEscalasCurso("MID", "SVCS")
val itsc2 = itsEscalasCurso("CLO", "SVCS")

//4 Itinerarios CLO-SVO RR SALIDAS EN ORDEN DISTINTO
val itsc3 = itsEscalasCurso("CLO", "SVO")

//2 Itinerarios CLO-MEX
val itsc4 = itsEscalasCurso("CLO", "MEX")

//2 Itinerarios CTG-PTY
val itsc5 = itsEscalasCurso("CTG", "PTY")



//Pruebas para 3.4 ENUNCIADO (Correspondiente a 2.1.4)
//Devuelven exactamente lo que pide el enunciado por tanto son correctas (eso espero xd)
val itsAireCurso = itinerariosAire(vuelosCurso, aeropuertosCurso)
//Pruebas itinerariosAire
val itsa1 = itsAireCurso("MID", "SVCS")
val itsa2 = itsAireCurso("CLO", "SVCS")

//4 Itinerarios CLO-SVO
val itsa3 = itsAireCurso("CLO", "SVO")

//2 Itinerarios CLO-MEX
val itsa4 = itsAireCurso("CLO", "MEX")

//2 Itinerarios CTG-PTY
val itsa5 = itsAireCurso("CTG", "PTY")


//Pruebas para 3.5 ENUNCIADO (Correspondiente a 2.1.5)
//Devuelven exactamente lo que pide el enunciado por tanto son correctas (eso espero xd)
val itsSalidaCurso = itinerarioSalida(vuelosCurso, aeropuertosCurso)
//Pruebas itinerariosSalida
val itsal1 = itsSalidaCurso("CTG", "PTY", 11, 40)
val itsal2 = itsSalidaCurso("CTG", "PTY", 11, 55)
val itsal3 = itsSalidaCurso("CTG", "PTY", 10, 30)



// ======================================================
// PRUEBAS VERSIONES PARALELAS vs SECUENCIALES
// Comparamos TODOS los casos que usamos en el enunciado.
// ======================================================

// --------- Casos origen/destino compartidos por F1–F4 ---------

val casosOD: List[(String, String)] = List(
  ("MID", "SVCS"),
  ("CLO", "SVCS"),
  ("CLO", "SVO"),
  ("CLO", "MEX"),
  ("CTG", "PTY")
)

// ---------------- F1 vs F1p: itinerarios / itinerariosPar ----------------

val itsCursoPar = itinerariosPar(vuelosCurso, aeropuertosCurso)

val chequeoF1: List[(String, Boolean)] =
  casosOD.map { case (o, d) =>
    val sec = itsCurso(o, d)
    val par = itsCursoPar(o, d)
    val ok  = (sec == par)
    (s"F1 itinerarios $o -> $d", ok)
  }

chequeoF1   // ver en el worksheet: todos deberían ser (…,true)


// -------------- F2 vs F2p: itinerariosTiempo / itinerariosTiempoPar --------------

val itsTiempoCursoPar = itinerariosTiempoPar(vuelosCurso, aeropuertosCurso)

val chequeoF2: List[(String, Boolean)] =
  casosOD.map { case (o, d) =>
    val sec = itsTiempoCurso(o, d)
    val par = itsTiempoCursoPar(o, d)
    val ok  = (sec == par)
    (s"F2 itinerariosTiempo $o -> $d", ok)
  }

chequeoF2   // todos deberían ser true


// -------------- F3 vs F3p: itinerariosEscalas / itinerariosEscalasPar --------------

val itsEscalasCursoPar = itinerariosEscalasPar(vuelosCurso, aeropuertosCurso)

val chequeoF3: List[(String, Boolean)] =
  casosOD.map { case (o, d) =>
    val sec = itsEscalasCurso(o, d)
    val par = itsEscalasCursoPar(o, d)
    val ok  = (sec == par)
    (s"F3 itinerariosEscalas $o -> $d", ok)
  }

chequeoF3   // todos deberían ser true


// -------------- F4 vs F4p: itinerariosAire / itinerariosAirePar --------------

val itsAireCursoPar = itinerariosAirePar(vuelosCurso, aeropuertosCurso)

val chequeoF4: List[(String, Boolean)] =
  casosOD.map { case (o, d) =>
    val sec = itsAireCurso(o, d)
    val par = itsAireCursoPar(o, d)
    val ok  = (sec == par)
    (s"F4 itinerariosAire $o -> $d", ok)
  }

chequeoF4   // todos deberían ser true


// -------------- F5 vs F5p: itinerarioSalida / itinerarioSalidaPar --------------

val itsSalidaCursoPar = itinerarioSalidaPar(vuelosCurso, aeropuertosCurso)

val chequeoF5: List[(String, Boolean)] =
  casosOD.map { case (o, d) =>
    val sec = itsSalidaCurso(o, d, 11, 40)
    val par = itsSalidaCursoPar(o, d, 11, 40)
    val ok  = (sec == par)
    (s"F5 itinerarioSalida $o -> $d", ok)
  }

chequeoF5  // efectivamente, todos dan true.



// =======================================================================
//                 PRUEBAS SECUENCIALES Y PARALELAS COMPLETAS
//                     Itinerarios – Programación Funcional
// =======================================================================


// -----------------------------------------------------------
// Helpers para mostrar itinerarios (óptimo, legible)
// -----------------------------------------------------------
def mostrar(it: Itinerario): Unit =
  if (it.isEmpty) println("   (sin vuelos)")
  else println("   " + it.map(v => s"${v.Org}->${v.Dst}(${v.Aln}${v.Num})").mkString(" | "))

def medir[T](bloque: => T): (T, Double) = {
  val t0 = System.nanoTime()
  val r  = bloque
  val t1 = System.nanoTime()
  (r, (t1 - t0) / 1e6)
}

def medirPromedio[T](bloque: => T, rep: Int): (T, Double, Double) = {
  // Ejecutamos una vez para obtener el resultado (puro)
  val resultado = bloque

  // Ejecutamos N veces solo para medir tiempo
  val tiempos: List[Double] =
    List.fill(rep)(medir(bloque)._2)

  val promedio = tiempos.sum / rep

  // desviación estándar
  val variance =
    tiempos.map(t => math.pow(t - promedio, 2)).sum / rep

  val desviacion = math.sqrt(variance)

  (resultado, promedio, desviacion)
}

def compararPromedio[T](sec: => T, par: => T, rep: Int):
(Double, Double, Double, Double, Boolean) = {

  val (resS, promS, desvS) = medirPromedio(sec, rep)
  val (resP, promP, desvP) = medirPromedio(par, rep)

  val speedup = promS / promP
  val ok = (resS == resP)

  (promS, promP, speedup, (desvS + desvP) / 2, ok)
}

def mostrarItinerariosEn(ds: List[Vuelo]): Boolean =
  ds.size <= 40      // solo imprimimos en dataset 15 y dataset 40



// =======================================================================
//  LISTA DE DATASETS
// =======================================================================

/*
val datasets15 = List(
  ("A1", vuelosA1),
  ("A2", vuelosA2),
  ("A3", vuelosA3),
  ("A4", vuelosA4),
  ("A5", vuelosA5)
)
*/


val datasets40 = List(
  ("B1", vuelosB1),
  ("B2", vuelosB2),
  ("B3", vuelosB3),
  ("B4", vuelosB4),
  ("B5", vuelosB5)
)

/*
val datasets100 = List(
  ("C1", vuelosC1),
  ("C2", vuelosC2),
  ("C3", vuelosC3),
  ("C4", vuelosC4),
  ("C5", vuelosC5)
)

val datasets500 = List(
  ("D1", vuelosD1),
  ("D2", vuelosD2),
  ("D3", vuelosD3),
  ("D4", vuelosD4),
  ("D5", vuelosD5)
)
*/


/*
val datasets40 = List(
  ("B1", vuelosB1),
  ("B2", vuelosB2),
  ("B3", vuelosB3),
  ("B4", vuelosB4),
  ("B5", vuelosB5)
)
*/

/*
val datasets100 = List(
  ("C1", vuelosC1),
  ("C2", vuelosC2),
  ("C3", vuelosC3),
  ("C4", vuelosC4),
  ("C5", vuelosC5)
)
*/

/*
val datasets500 = List(
  ("D1", vuelosD1),
  ("D2", vuelosD2),
  ("D3", vuelosD3)
)
*/

//val dataset200:List[(String, List[Vuelo])] = List(("200", vuelosC1 ++ vuelosC2))
val extra:List[Vuelo] = List(Vuelo("HP", 200, "PHX", 8, 10, "ABQ", 12, 0, 0))

val dataset16:List[(String, List[Vuelo])] = List(("16", vuelosA1 ++ extra))

// Aeropuertos (los de USA)
val aerop = aeropuertos

// =======================================================================
//         FUNCIÓN PARA OBTENER LOS O-D DE UN DATASET
// =======================================================================

def obtenerParesOD(ds: List[Vuelo]): List[(String,String)] = {
  val aeropuertosUsados: Set[String] =
    ds.map(_.Org).toSet ++ ds.map(_.Dst).toSet

  println("\nAeropuertos presentes:")
  println(aeropuertosUsados.mkString("  "))

  val pares =
    (for {
      o <- aeropuertosUsados
      d <- aeropuertosUsados
      if o != d
    } yield (o, d)).toList

  println("\nNúmero de pares O-D a probar: " + pares.size)
  pares
}

// =======================================================================
//                    FUNCIÓN DE PRUEBA PARA UN O-D
// =======================================================================

def probarOD(ori: String, dst: String, ds: List[Vuelo], rep: Int): (Double, Double, Double) = {
  println(s"\n===================================================")
  println(s" PRUEBA O-D:  $ori → $dst")
  println(s"===================================================\n")

  val imprimir = mostrarItinerariosEn(ds)

  def fila(nombre: String, s: Double, p: Double, sp: Double, ok: Boolean): Unit =
    println(f"${nombre}%-10s  sec = ${s}%-8.2f ms   par = ${p}%-8.2f ms   speedup = ${sp}%-6.2f   ok?: $ok")

  // ---------------- F1 ----------------
  val (t1s, t1p, sp1, desv1, ok1) = compararPromedio(
    itinerarios(ds, aerop)(ori, dst),
    itinerariosPar(ds, aerop)(ori, dst),
    rep
  )
  fila("F1", t1s, t1p, sp1, ok1)

  if (imprimir) {
    println("\n--- Itinerarios (F1) ---")
    println("Secuencial:")
    itinerarios(ds, aerop)(ori, dst).take(3).foreach(mostrar)

    println("Paralelo:")
    itinerariosPar(ds, aerop)(ori, dst).take(3).foreach(mostrar)

    println("------------------------\n")
  }

  // ---------------- F2 ----------------
  val (t2s, t2p, sp2, desv2, ok2) = compararPromedio(
    itinerariosTiempo(ds, aerop)(ori, dst),
    itinerariosTiempoPar(ds, aerop)(ori, dst),
    rep
  )
  fila("F2", t2s, t2p, sp2, ok2)

  // ---------------- F3 ----------------
  val (t3s, t3p, sp3, desv3, ok3) = compararPromedio(
    itinerariosEscalas(ds, aerop)(ori, dst),
    itinerariosEscalasPar(ds, aerop)(ori, dst),
    rep
  )
  fila("F3", t3s, t3p, sp3, ok3)

  // ---------------- F4 ----------------
  val (t4s, t4p, sp4, desv4, ok4) = compararPromedio(
    itinerariosAire(ds, aerop)(ori, dst),
    itinerariosAirePar(ds, aerop)(ori, dst),
    rep
  )
  fila("F4", t4s, t4p, sp4, ok4)

  // ---------------- F5 ----------------
  val (t5s, t5p, sp5, desv5, ok5) = compararPromedio(
    itinerarioSalida(ds, aerop)(ori, dst, 18, 30),
    itinerarioSalidaPar(ds, aerop)(ori, dst, 18, 30),
    rep
  )
  fila("F5", t5s, t5p, sp5, ok5)

  println("\n===================================================")

  // ⬅⬅⬅ Aquí devolvemos promedios de TODA la prueba O-D
  val promSecTotal = (t1s + t2s + t3s + t4s + t5s) / 5
  val promParTotal = (t1p + t2p + t3p + t4p + t5p) / 5
  val desvTotal = (desv1 + desv2 + desv3 + desv4 + desv5) / 5

  (promSecTotal, promParTotal, desvTotal)
}



// =======================================================================
//                    EJECUTAR TODAS LAS PRUEBAS A1–A5
// =======================================================================

datasets40.foreach { case (nombre, ds) =>

  println("\n=======================================================")
  println(s"==============   DATASET $nombre   =====================")
  println("=======================================================\n")

  val pares = obtenerParesOD(ds)

  // Para caso de 500, adaptar las repeticiones a 5.
  val resultados: List[(Double, Double, Double)] =
    pares.take(3).map { case (o, d) =>
      probarOD(o, d, ds, rep = 50) // ← devuelve (promSec, promPar, desv)
    }

  // Para no imprimir 400 líneas:
  pares.take(3).foreach { case (o, d) => probarOD(o, d, ds) }

  println(s"\n(Pruebas mostradas: 3 / ${pares.size})")
}

  println(s"\n(Pruebas mostradas: 3 / ${pares.size})")
  println(f"Tiempo promedio SEC del grupo: $promSecGrupo%.2f ms")
  println(f"Tiempo promedio PAR del grupo: $promParGrupo%.2f ms")
  println(f"Desviación estándar promedio: $desvGrupo%.2f ms")

  println("\n-------------------------------------------------------")
}
