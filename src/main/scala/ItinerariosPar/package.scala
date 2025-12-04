import Datos._
import common._          // parallel / task: paralelismo de tareas
import Itinerarios._     // reutilizamos helpers y versiones secuenciales

/**
 *
 * Contiene las versiones **paralelas** de las funciones del paquete Itinerarios:
 *   - F1p: itinerariosPar        (2.1.1)  → todos los itinerarios (DFS paralelo).
 *   - F2p: itinerariosTiempoPar  (2.1.2)  → minimiza tiempo total de viaje.
 *   - F3p: itinerariosEscalasPar (2.1.3)  → minimiza número de escalas.
 *   - F4p: itinerariosAirePar    (2.1.4)  → minimiza tiempo en el aire.
 *   - F5p: itinerarioSalidaPar   (2.1.5)  → optimiza la hora de salida.
 *
 * Todas las funciones hacen esencialmente lo mismo que sus versiones secuenciales,
 * pero usando principalmente **paralelismo de tareas** (parallel) y, donde tiene
 * sentido, paralelismo de datos (map paralelo por divide-and-conquer).
 */
package object ItinerariosPar {

  // =========================
  // Helpers paralelos
  // =========================

  /**
   * Umbral genérico para decidir si vale la pena crear tareas.
   * Listas con tamaño <= UMBRAL_LISTA se procesan secuencialmente.
   */
  private val UMBRAL_LISTA = 8

  /**
   * mapPar – Versión paralela de map sobre listas.
   *
   * Recibe:
   *   xs → lista de entrada.
   *   f  → función a aplicar a cada elemento.
   *
   * Estrategia (paralelismo de tareas + datos):
   *   - Si la lista es pequeña (<= UMBRAL_LISTA), usamos xs.map(f).
   *   - Si es grande, la partimos en dos mitades (izq, der) y aplicamos
   *     recursivamente mapPar a cada mitad en **tareas paralelas** usando `parallel`.
   *   - Al final concatenamos los resultados.
   */
  private def mapPar[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case _ if xs.length <= UMBRAL_LISTA =>
      xs.map(f)
    case _ =>
      val (izq, der) = xs.splitAt(xs.length / 2)
      val (resIzq, resDer) = parallel(
        mapPar(izq)(f),
        mapPar(der)(f)
      )
      resIzq ::: resDer
  }

  // =========================
  // F1p: itinerariosPar (2.1.1)
  // =========================

  /**
   * F1p – Encontrando itinerarios en paralelo.
   *
   * Versión paralela de F1 (itinerarios).
   *
   * Construye una función que, dados dos códigos de aeropuerto (cod1, cod2),
   * devuelve TODOS los itinerarios posibles (listas de vuelos) que van de
   * cod1 a cod2, sin repetir aeropuertos, pero usando **paralelismo de tareas**
   * para explorar el árbol de búsqueda (DFS).
   *
   * Idea:
   *   - Reutilizamos la misma estructura de búsqueda en profundidad que en
   *     la versión secuencial.
   *   - Para cada aeropuerto, tomamos la lista de vuelos salientes y la
   *     exploramos con una función recursiva que:
   *        * para listas pequeñas, recorre secuencialmente;
   *        * para listas grandes, divide en dos mitades y usa `parallel`
   *          para explorar cada mitad en una tarea distinta.
   */
  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    // Preprocesamos los vuelos una sola vez: origen -> lista de vuelos
    val porOrigen: Map[String, List[Vuelo]] = vuelos.groupBy(_.Org)

    /**
     * DFS paralelo que construye todos los itinerarios.
     *
     * Parámetros:
     *   origen    → aeropuerto actual.
     *   destino   → aeropuerto destino final.
     *   visitados → conjunto de aeropuertos ya visitados
     *               en el camino actual (para evitar ciclos).
     */
    def buscar(origen: String, destino: String, visitados: Set[String]): List[Itinerario] =
      if (origen == destino) {
        // Caso base: ya llegamos al destino → camino vacío.
        List(Nil)
      } else {
        // Vuelos que salen de "origen" y no llevan a aeropuertos ya visitados
        val salientes =
          porOrigen.filter {case (k, _) => k == origen}.values.toList match {
            case Nil => Nil
            case vs :: _ => vs.filter(v => !visitados(v.Dst))
          }

        /**
         * Explora una lista de vuelos salientes.
         *
         * Si la lista es pequeña, se procesa secuencialmente.
         * Si es grande, se parte en dos sublistas, y cada una se explora
         * en una tarea distinta usando `parallel`.
         */
        def exploraLista(vs: List[Vuelo]): List[Itinerario] = vs match {
          case Nil => Nil
          case _ if vs.length <= UMBRAL_LISTA =>
            // Secuencial
            vs.flatMap { v =>
              buscar(v.Dst, destino, visitados + v.Dst).map(it => v :: it)
            }
          case _ =>
            // Paralelizamos en dos tareas
            val (izq, der) = vs.splitAt(vs.length / 2)
            val (itsIzq, itsDer) = parallel(
              exploraLista(izq),
              exploraLista(der)
            )
            itsIzq ::: itsDer
        }

        exploraLista(salientes)
      }

    // Función que el usuario invoca
    (c1: String, c2: String) =>
      if (c1 == c2) List(Nil) else buscar(c1, c2, Set(c1))
  }

  // =========================
  // F2p: itinerariosTiempoPar (2.1.2)
  // =========================

  /**
   * F2p – Minimización de tiempo total de viaje (versión paralela).
   *
   * Versión paralela de F2 (itinerariosTiempo).
   *
   * Construye una función que, dados dos códigos de aeropuerto (cod1, cod2),
   * devuelve HASTA tres itinerarios con menor tiempo total de viaje,
   * calculando en **paralelo** el tiempo total de cada itinerario.
   *
   * Tipo de paralelismo:
   *   - Paralelismo de tareas + datos usando `mapPar` sobre la lista de
   *     itinerarios.
   */
  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    // Mapa códigoAeropuerto -> GMT, para usar en tiempoTotal
    val gmtMap: Map[String, Int] =
      mapaAeropuertos(aeropuertos).map { case (cod, aero) => cod -> aero.GMT }

    // Reutilizamos la versión paralela de F1
    val todosItinerariosPar = itinerariosPar(vuelos, aeropuertos)

    (c1: String, c2: String) => {
      val its = todosItinerariosPar(c1, c2)

      // mapPar: calculamos en paralelo (it, tiempoTotal(it))
      val calificados: List[(Itinerario, Int)] =
        mapPar(its) { it => (it, tiempoTotal(it, gmtMap)) }

      val ordenados: List[Itinerario] =
        calificados.sortBy(_._2).map(_._1)

      primerosN(3, ordenados)
    }
  }

  // =========================
  // F3p: itinerariosEscalasPar (2.1.3)
  // =========================

  /**
   * F3p – Minimización del número de escalas (versión paralela).
   *
   * Versión paralela de F3 (itinerariosEscalas).
   *
   * Devuelve hasta 3 itinerarios entre cod1 y cod2 con menor cantidad
   * de escalas totales (cambios de avión + escalas técnicas).
   *
   * Criterios de ordenamiento (mismos que la versión secuencial):
   *   1) Menor escalasTotales(it).
   *   2) Menor número de vuelos (it.length).
   *   3) Menor tiempo total de viaje (tiempoTotal).
   *
   * Tipo de paralelismo:
   *   - Paralelismo de tareas + datos: usamos `mapPar` para calcular en
   *     paralelo las métricas de cada itinerario.
   */
  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    val todosItinerariosPar = itinerariosPar(vuelos, aeropuertos)

    // Mapa GMT para poder usar tiempoTotal como desempate
    val gmtMap: Map[String, Int] =
      mapaAeropuertos(aeropuertos).map { case (cod, aero) => cod -> aero.GMT }

    (c1: String, c2: String) => {
      val its = todosItinerariosPar(c1, c2)

      // mapPar: calculamos en paralelo las tuplas (it, (escalas, len, tiempo))
      val calificados: List[(Itinerario, (Int, Int, Int))] =
        mapPar(its) { it =>
          val esc  = escalasTotales(it)
          val len  = it.length
          val tTot = tiempoTotal(it, gmtMap)
          (it, (esc, len, tTot))
        }

      val ordenados: List[Itinerario] =
        calificados.sortBy(_._2).map(_._1)

      primerosN(3, ordenados)
    }
  }

  // =========================
  // F4p: itinerariosAirePar (2.1.4)
  // =========================

  /**
   * F4p – Minimización del tiempo en el aire (versión paralela).
   *
   * Versión paralela de F4 (itinerariosAire).
   *
   * Devuelve hasta 3 itinerarios entre cod1 y cod2 que minimizan
   * la suma de distancias entre aeropuertos (tiempo en aire).
   *
   * Criterios de ordenamiento (mismos que la versión secuencial):
   *   1) Menor tiempo en el aire (tiempoAireTotal).
   *   2) Menos escalasTotales.
   *   3) Menor tiempo total de viaje (tiempoTotal).
   *
   * Tipo de paralelismo:
   *   - Paralelismo de tareas + datos: usamos `mapPar` para calcular en
   *     paralelo las métricas de “tiempo en aire”.
   */
  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    val aeroMap = mapaAeropuertos(aeropuertos)
    val gmtMap: Map[String, Int] =
      mapaAeropuertos(aeropuertos).map { case (cod, aero) => cod -> aero.GMT }

    val todosItinerariosPar = itinerariosPar(vuelos, aeropuertos)

    (c1: String, c2: String) => {
      val its = todosItinerariosPar(c1, c2)

      // mapPar: calculamos en paralelo (it, (aire, escalas, tiempo))
      val calificados: List[(Itinerario, (Double, Int, Int))] =
        mapPar(its) { it =>
          val aire = tiempoAireTotal(it, aeroMap)
          val esc  = escalasTotales(it)
          val tTot = tiempoTotal(it, gmtMap)
          (it, (aire, esc, tTot))
        }

      val ordenados: List[Itinerario] =
        calificados.sortBy(_._2).map(_._1)

      primerosN(3, ordenados)
    }
  }

  // =========================
  // F5p: itinerarioSalidaPar (2.1.5)
  // =========================

  /**
   * F5p – Optimización de la hora de salida (versión paralela).
   *
   * Versión paralela de F5 (itinerarioSalida).
   *
   * Construye una función que, dados:
   *   - cod1: aeropuerto de origen,
   *   - cod2: aeropuerto de destino,
   *   - hCita, mCita: hora local de la cita en el aeropuerto de destino,
   *
   * selecciona un itinerario que:
   *   1. (Siguiendo la interpretación del foro del profesor) se considera
   *      que llega algún día antes de la cita, por lo que todos los
   *      itinerarios cod1 → cod2 son candidatos.
   *   2. Entre todos los candidatos, se elige el que tiene la **hora de
   *      salida en cod1 lo más tarde posible**.
   *
   * Si no hay itinerarios entre cod1 y cod2, devuelve el itinerario vacío (Nil).
   *
   * Tipo de paralelismo:
   *   - Paralelismo de tareas + datos: usamos `mapPar` para calcular en
   *     paralelo, para cada itinerario, su hora de salida en UTC.
   */
  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String, Int, Int) => Itinerario = {

    // Mapa código -> GMT para convertir horas locales a UTC
    val gmtMap: Map[String, Int] =
      mapaAeropuertos(aeropuertos).map { case (cod, aero) => cod -> aero.GMT }

    // Generamos los itinerarios usando la versión paralela de F1
    val todosItinerariosPar = itinerariosPar(vuelos, aeropuertos)

    (c1: String, c2: String, hCita: Int, mCita: Int) => {

      // Hora de la cita en UTC, usando el GMT del aeropuerto de destino
      val citaUTC = utcMinutes(hCita, mCita, gmtMap(c2))

      val its = todosItinerariosPar(c1, c2)

      // mapPar: para cada itinerario calculamos en paralelo su hora de salida dep0.
      val candidatos: List[(Itinerario, Int)] =
        mapPar(its) {
          case Nil =>
            // Itinerario vacío: solo tiene sentido cuando origen = destino.
            if (c1 == c2) (Nil, citaUTC)
            else         (Nil, Int.MinValue) // marcador “no válido”

          case list =>
            val dep0    = utcMinutes(list.head.HS, list.head.MS, gmtMap(list.head.Org))
            val arr0Raw = utcMinutes(list.head.HL, list.head.ML, gmtMap(list.head.Dst))
            val arr0Adj = adjust(dep0, arr0Raw)

            val (_, lastArr) = list.tail.foldLeft((dep0, arr0Adj)) {
              case ((_, prevArr), vuelo) =>
                val depRaw = utcMinutes(vuelo.HS, vuelo.MS, gmtMap(vuelo.Org))
                val depAdj = adjust(prevArr, depRaw)
                val arrRaw = utcMinutes(vuelo.HL, vuelo.ML, gmtMap(vuelo.Dst))
                val arrAdj = adjust(depAdj, arrRaw)
                (depAdj, arrAdj)
            }

            // Todos los itinerarios que llegan a cod2 son considerados;
            // usamos dep0 (hora de salida) para escoger el “más tarde”.
            (list, dep0)
        }.filter {
          // Filtramos el marcador de “no válido” cuando c1 != c2.
          case (Nil, dep) => dep != Int.MinValue
          case _          => true
        }

      candidatos match {
        case Nil => Nil
        case _ =>
          val (mejorItinerario, _) =
            candidatos.maxBy { case (_, depUTC) => depUTC }
          mejorItinerario
      }
    }
  }
}
