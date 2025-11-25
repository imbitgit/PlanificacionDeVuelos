import Datos._
import Itinerarios._
import ItinerariosPar._


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

//4 Itinerarios CLO-SVO RR, SALIDA DIFERENTE 
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
val itsal3 = itsSalidaCurso("CTG", "PTY", 10, 30) //RR DEVUELVE LISTA VACIA, ALGO Q NO DEBE SER.




