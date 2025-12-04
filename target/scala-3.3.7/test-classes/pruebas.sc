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

/*
// ================================================================
//                     PRUEBAS CON EL CURSO PEQUEÑO
// ================================================================
println("\n================= PRUEBAS CURSO =================")

val itsCursoN = itinerarios(vuelosCurso, aeropuertosCurso)

val itsC1 = itsCursoN("MID", "SVCS")
val itsC2 = itsCursoN("CLO", "SVCS")
val itsC3 = itsCursoN("CLO", "SVO")
val itsC4 = itsCursoN("CLO", "MEX")
val itsC5 = itsCursoN("CTG", "PTY")

println("\n--- Itinerarios Curso ---")
println("MID → SVCS:"); itsC1.foreach(mostrar)
println("\nCLO → SVCS:"); itsC2.foreach(mostrar)
println("\nCLO → SVO (4 itinerarios):"); itsC3.foreach(mostrar)
println("\nCLO → MEX (2 itinerarios):"); itsC4.foreach(mostrar)
println("\nCTG → PTY (2 itinerarios):"); itsC5.foreach(mostrar)

// ---------------------------------------------------------------
//      PRUEBA DE LAS 4 FUNCIONES ÓPTIMAS EN EL CURSO
// ---------------------------------------------------------------
val fTpoCur = itinerariosTiempo(vuelosCurso, aeropuertosCurso)
val fEscCur = itinerariosEscalas(vuelosCurso, aeropuertosCurso)
val fAirCur = itinerariosAire(vuelosCurso, aeropuertosCurso)
val fSalCur = itinerarioSalida(vuelosCurso, aeropuertosCurso)

println("\n--- Tiempo Curso CLO → SVO ---"); fTpoCur("CLO","SVO").foreach(mostrar)
println("\n--- Escalas Curso CLO → SVO ---"); fEscCur("CLO","SVO").foreach(mostrar)
println("\n--- Aire Curso CLO → SVO ---"); fAirCur("CLO","SVO").foreach(mostrar)
println("\n--- Salida Curso CTG → PTY (cita 11:40) ---"); mostrar(fSalCur("CTG","PTY",11,40))
*/

// =======================================================================
//                             PRUEBAS ESCALADAS
//              15 vuelos  →  40 vuelos  →  100 vuelos  →  200+ vuelos
// =======================================================================

// =======================================================
//           LISTAS DE DATASETS (A, B, C, D, …)
// =======================================================
/*
val datasets15  = List(("A1", vuelosA1), ("A2", vuelosA2), ("A3", vuelosA3), ("A4", vuelosA4), ("A5", vuelosA5))
val datasets40  = List(("B1", vuelosB1), ("B2", vuelosB2), ("B3", vuelosB3), ("B4", vuelosB4), ("B5", vuelosB5))
val datasets100 = List(("C1", vuelosC1), ("C2", vuelosC2), ("C3", vuelosC3), ("C4", vuelosC4), ("C5", vuelosC5))

val dataset200 = ("200v", vuelosC1 ++ vuelosC2)
val dataset300 = ("300v", vuelosC1 ++ vuelosC2 ++ vuelosC3)
val dataset400 = ("400v", vuelosC1 ++ vuelosC2 ++ vuelosC3 ++ vuelosC4)
val dataset500 = ("500v", vuelosC1 ++ vuelosC2 ++ vuelosC3 ++ vuelosC4 ++ vuelosC5)

// =======================================================
//               FUNCIÓN DE PRUEBA GENERAL
// =======================================================
def probar(nombre: String, vuelos: List[Vuelo], ori: String, dst: String): Unit = {
  println("\n" + "="*90)
  println(s"DATASET $nombre — ${vuelos.length} vuelos")
  println("="*90)

  println("\n--- VERSIÓN SECUENCIAL ---")
  val f1 = medir("F1 itinerarios") {
    itinerarios(vuelos, aeropuertosCurso)(ori, dst)
  }
  println(s"     Cantidad rutas: ${f1.length}")

  val f2 = medir("F2 tiempo") {
    itinerariosTiempo(vuelos, aeropuertosCurso)(ori, dst)
  }
  println("     Mejores 3 por tiempo:"); f2.foreach(mostrar)

  val f3 = medir("F3 escalas") {
    itinerariosEscalas(vuelos, aeropuertosCurso)(ori, dst)
  }
  println("     Mejores 3 por escalas:"); f3.foreach(mostrar)

  val f4 = medir("F4 aire") {
    itinerariosAire(vuelos, aeropuertosCurso)(ori, dst)
  }
  println("     Mejores 3 por aire:"); f4.foreach(mostrar)

  val f5 = medir("F5 salida") {
    itinerarioSalida(vuelos, aeropuertosCurso)(ori, dst, 18, 30)
  }
  println("     Mejor salida antes de la cita:")
  mostrar(f5)

  println("\n--- VERSIÓN PARALELA ---")
  val fp1 = medir("F1p itinerariosPar") {
    itinerariosPar(vuelos, aeropuertosCurso)(ori, dst)
  }
  println(s"     Cantidad rutas: ${fp1.length}")

  val fp2 = medir("F2p tiempoPar") {
    itinerariosTiempoPar(vuelos, aeropuertosCurso)(ori, dst)
  }
  println("     Mejores 3 por tiempo:"); fp2.foreach(mostrar)

  val fp3 = medir("F3p escalasPar") {
    itinerariosEscalasPar(vuelos, aeropuertosCurso)(ori, dst)
  }
  println("     Mejores 3 por escalas:"); fp3.foreach(mostrar)

  val fp4 = medir("F4p airePar") {
    itinerariosAirePar(vuelos, aeropuertosCurso)(ori, dst)
  }
  println("     Mejores 3 por aire:"); fp4.foreach(mostrar)

  val fp5 = medir("F5p salidaPar") {
    itinerarioSalidaPar(vuelos, aeropuertosCurso)(ori, dst, 18, 30)
  }
  println("     Mejor salida antes de la cita:")
  mostrar(fp5)
}

// =======================================================
//                          EJECUCIÓN
// =======================================================
println("\n================ PRUEBAS 15 VUELOS ================")
datasets15.foreach { case (n, v) => probar(n, v, "HOU", "MSY") }

println("\n================ PRUEBAS 40 VUELOS ================")
datasets40.foreach { case (n, v) => probar(n, v, "DFW", "ORD") }

println("\n================ PRUEBAS 100 VUELOS ================")
datasets100.foreach { case (n, v) => probar(n, v, "ORD", "TPA") }

println("\n================ PRUEBAS GRANDES (200–500) ================")
probar(dataset200._1, dataset200._2, "ORD", "TPA")
probar(dataset300._1, dataset300._2, "ORD", "TPA")
probar(dataset400._1, dataset400._2, "ORD", "TPA")
probar(dataset500._1, dataset500._2, "ORD", "TPA")
*/


// =======================================================================
//  LISTA DE DATASETS (A1..A5) — USAMOS LOS QUE YA DEFINISTE EN Datos
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

// Aeropuertos (los de USA que ya tienes)
val aerop = aeropuertos

// =======================================================================
//         FUNCIÓN PARA OBTENER LOS O-D DE UN DATASET DE 15 VUELOS
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
//                (IGUAL A TU FORMATO, SIN CAMBIAR NADA)
// =======================================================================

def probarOD(ori: String, dst: String, ds: List[Vuelo]): Unit = {
  println(s"\n===================================================")
  println(s" PRUEBA O-D:  $ori → $dst")
  println(s"===================================================\n")

  // ---------------- F1 ----------------
  val (r1s, t1s) = medir { itinerarios(ds, aerop)(ori, dst) }
  val (r1p, t1p) = medir { itinerariosPar(ds, aerop)(ori, dst) }
  val ok1 = (r1s == r1p)

  // ---------------- F2 ----------------
  val (r2s, t2s) = medir { itinerariosTiempo(ds, aerop)(ori, dst) }
  val (r2p, t2p) = medir { itinerariosTiempoPar(ds, aerop)(ori, dst) }
  val ok2 = (r2s == r2p)

  // ---------------- F3 ----------------
  val (r3s, t3s) = medir { itinerariosEscalas(ds, aerop)(ori, dst) }
  val (r3p, t3p) = medir { itinerariosEscalasPar(ds, aerop)(ori, dst) }
  val ok3 = (r3s == r3p)

  // ---------------- F4 ----------------
  val (r4s, t4s) = medir { itinerariosAire(ds, aerop)(ori, dst) }
  val (r4p, t4p) = medir { itinerariosAirePar(ds, aerop)(ori, dst) }
  val ok4 = (r4s == r4p)

  // ---------------- F5 ----------------
  val (r5s, t5s) = medir { itinerarioSalida(ds, aerop)(ori, dst, 18, 30) }
  val (r5p, t5p) = medir { itinerarioSalidaPar(ds, aerop)(ori, dst, 18, 30) }
  val ok5 = (r5s == r5p)

  // ---------------- TABLA FINAL ----------------
  def fila(n: String, s: Double, p: Double, ok: Boolean): Unit =
    println(f"${n}%-12s  sec = ${s}%-8.2f ms   par = ${p}%-8.2f ms   correctos?: $ok")

  fila("F1", t1s, t1p, ok1)
  fila("F2", t2s, t2p, ok2)
  fila("F3", t3s, t3p, ok3)
  fila("F4", t4s, t4p, ok4)
  fila("F5", t5s, t5p, ok5)
}


// =======================================================================
//                    EJECUTAR TODAS LAS PRUEBAS A1–A5
// =======================================================================

datasets40.foreach { case (nombre, ds) =>

  println("\n=======================================================")
  println(s"==============   DATASET $nombre   =====================")
  println("=======================================================\n")

  val pares = obtenerParesOD(ds)

  // Si quieres TODAS:
  // pares.foreach { case (o, d) => probarOD(o, d, ds) }

  // Para no imprimir 400 líneas:
  pares.take(3).foreach { case (o, d) => probarOD(o, d, ds) }

  println(s"\n(Pruebas mostradas: 3 / ${pares.size})")
}


