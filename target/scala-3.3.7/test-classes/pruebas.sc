import Datos._
import Itinerarios._
import ItinerariosPar._

vuelosCurso.take(2)  // Prueba para comprobar que sí hay funcionamiento.

/*
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

*/



