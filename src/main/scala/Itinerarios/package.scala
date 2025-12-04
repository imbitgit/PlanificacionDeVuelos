import Datos._

/**
 * Package object Itinerarios
 *
 *    Version Secuencial de las Funciones
 *   - Usa recursión (DFS) para generar itinerarios.
 *   - Usa reconocimiento de patrones (pattern matching con `case`).
 *   - Usa funciones de alto orden: map, flatMap, filter, foldLeft, sortBy, maxBy, collect.
 *   - Usa expresiones for para construir listas de itinerarios.
 *   - Encapsula lógica compleja en funciones auxiliares (helpers y funciones internas).
 *
 * Contiene:
 *   - Helpers comunes para tiempos, escalas y distancias.
 *   - F1: itinerarios        (2.1.1)  → todos los itinerarios.
 *   - F2: itinerariosTiempo  (2.1.2)  → minimiza tiempo total de viaje.
 *   - F3: itinerariosEscalas (2.1.3)  → minimiza número de escalas.
 *   - F4: itinerariosAire    (2.1.4)  → minimiza tiempo en el aire.
 *   - F5: itinerarioSalida   (2.1.5)  → optimiza la hora de salida.
 */
package object Itinerarios {

  // ======================================================
  // Helpers comunes
  // ======================================================

  /**
   * Construye un mapa códigoAeropuerto -> Aeropuerto.
   *
   * Evidencia de programación funcional:
   *   - Uso de foldLeft con reconocimiento de patrones sobre la tupla (acc, a).
   * Se usa en:
   *   - (2.1.2) itinerariosTiempo  → para obtener GMT.
   *   - (2.1.4) itinerariosAire    → para obtener coordenadas X,Y.
   *   - (2.1.5) itinerarioSalida   → para convertir horas a UTC.
   */
  def mapaAeropuertos(aeropuertos: List[Aeropuerto]): Map[String, Aeropuerto] =
    aeropuertos.foldLeft(Map[String, Aeropuerto]()) {
      // patrón (acc, a): acc = mapa acumulado, a = aeropuerto actual
      case (acc, a) => acc + (a.Cod -> a)
    }

  /**
   * Agrupa los vuelos por aeropuerto de origen.
   *
   * Evidencia de programación funcional:
   *   - Uso de colecciones inmutables y función de orden superior groupBy.
   *
   * Resultado:
   *   Map(org -> List(vuelos que salen de org)).
   *
   * Se usa en:
   *   - (2.1.1) itinerarios  → para obtener rápidamente vuelos saliendo de un origen.
   */
  private def vuelosPorOrigen(vuelos: List[Vuelo]): Map[String, List[Vuelo]] =
    vuelos.groupBy(_.Org)

  /**
   * Convierte una hora local (h:m) a minutos UTC, usando el GMT del aeropuerto.
   *
   * Relación:
   *   horaLocal = horaUTC + offset  ⇒  horaUTC = horaLocal - offset
   *
   * Se usa en:
   *   - (2.1.2) tiempoTotal / itinerariosTiempo.
   *   - (2.1.5) itinerarioSalida (cálculo con hora de cita y salidas).
   */
  def utcMinutes(h: Int, m: Int, gmt: Int): Int = {
    val offsetHours = gmt / 100             // -500 → -5, 300 → 3
    h * 60 + m - offsetHours * 60           // pasamos a minutos
  }

  /** Minutos que tiene un día completo (24 horas). */
  val DayMinutes: Int = 24 * 60

  /**
   * Ajusta un tiempo t para que sea mayor o igual que "after",
   * sumando días completos (de 24h) si es necesario.
   *
   * Se usa en:
   *   - (2.1.2) tiempoTotal / itinerariosTiempo.
   *   - (2.1.5) itinerarioSalida (para cadenas de vuelos).
   *
   * Casos típicos:
   *   - Vuelos que cruzan medianoche.
   *   - Vuelos que duran más de un día.
   */
  def adjust(after: Int, t: Int): Int =
    if (t >= after) t
    else {
      val delta = after - t
      // número mínimo de días completos que hay que sumar
      val days  = (delta + DayMinutes - 1) / DayMinutes
      t + days * DayMinutes
    }

  /**
   * Devuelve los primeros n elementos de una lista (o la lista completa
   * si tiene menos de n elementos).
   *
   * Evidencia de programación funcional:
   *   - Uso de pattern matching sobre (n, xs).
   *   - Recursión estructural sobre listas (case list).
   *
   * Se usa en:
   *   - (2.1.2) itinerariosTiempo.
   *   - (2.1.3) itinerariosEscalas.
   *   - (2.1.4) itinerariosAire.
   */
  def primerosN[A](n: Int, xs: List[A]): List[A] = (n, xs) match {
    case (_, Nil)   => Nil                    // lista vacía → nada que devolver
    case (0, _)     => Nil                    // ya tomamos n elementos
    case (k, list) => list.head :: primerosN(k - 1, list.tail)  // patrón cabeza/cola
  }

  /*
   * Número total de escalas de un itinerario.
   *
   * Evidencia de programación funcional:
   *   - Reconocimiento de patrones: case Nil / case _.
   *   - Uso de map + sum sobre una lista de vuelos.
   *
   * Se usa en:
   *   - (2.1.3) itinerariosEscalas.
   *   - (2.1.4) itinerariosAire (como criterio de desempate).
   *
   * Fórmula:
   *   escalasTotales = (número de cambios de avión) + sum(Esc de cada vuelo)
   *                  = (it.length - 1) + it.map(_.Esc).sum  (si no es vacío).
   */
  def escalasTotales(it: Itinerario): Int = it match {
    case Nil => 0
    case _   => (it.length - 1) + it.map(_.Esc).sum
  }

  /**
   * Distancia geométrica entre dos aeropuertos (en el plano X,Y).
   *
   * Se usa en:
   *   - (2.1.4) tiempoAireTotal / itinerariosAire.
   */
  def distancia(a1: Aeropuerto, a2: Aeropuerto): Double = {
    val dx = a1.X - a2.X
    val dy = a1.Y - a2.Y
    math.sqrt(dx.toDouble * dx.toDouble + dy.toDouble * dy.toDouble)
  }

  /**
   * Tiempo total "en el aire" para un itinerario.
   *
   * Evidencia de programación funcional:
   *   - Uso de foldLeft con reconocimiento de patrones (acum, vuelo).
   *   - Uso de función auxiliar distancia.
   *
   * Modela el tiempo de vuelo como la suma de distancias entre:
   *   origen(vuelo) → destino(vuelo) para cada vuelo.
   *
   * Lo usan:
   *   - (2.1.4) itinerariosAire (secuencial).
   *   - itinerariosAirePar 
   */
  def tiempoAireTotal(it: Itinerario, aeroMap: Map[String, Aeropuerto]): Double =
    it.foldLeft(0.0) {
      case (acum, vuelo) =>
        val origen  = aeroMap(vuelo.Org)
        val destino = aeroMap(vuelo.Dst)
        acum + distancia(origen, destino)
    }

  /**
   * Calcula el tiempo total de viaje de un itinerario en minutos.
   *
   * Evidencia de programación funcional:
   *   - Reconocimiento de patrones en listas: case Nil / case list.
   *   - Uso de foldLeft con pattern matching sobre tuplas.
   *   - Uso de helpers utcMinutes y adjust.
   *
   * El tiempo total incluye:
   *   - Tiempo de vuelo.
   *   - Tiempos de espera entre conexiones.
   *   - Diferencias de huso horario (GMT) entre aeropuertos.
   */
  def tiempoTotal(it: Itinerario, gmtMap: Map[String, Int]): Int = it match {
    case Nil => 0

    case list =>
      // Salida del primer vuelo en UTC
      val dep0    = utcMinutes(list.head.HS, list.head.MS, gmtMap(list.head.Org))
      // Llegada del primer vuelo en UTC, ajustada para no quedar antes de la salida
      val arr0Adj = adjust(dep0,
        utcMinutes(list.head.HL, list.head.ML, gmtMap(list.head.Dst)))

      // Recorremos el resto de vuelos actualizando salida y llegada
      val (_, lastArr) =
        list.tail.foldLeft((dep0, arr0Adj)) {
          // patrón ((_, prevArr), v): ignoramos la última salida y usamos solo la última llegada
          case ((_, prevArr), v) =>
            val depAdj = adjust(prevArr,
              utcMinutes(v.HS, v.MS, gmtMap(v.Org)))
            val arrAdj = adjust(depAdj,
              utcMinutes(v.HL, v.ML, gmtMap(v.Dst)))
            (depAdj, arrAdj)
        }

      // Tiempo total = última llegada - primera salida
      lastArr - dep0
  }

  // ======================================================
  // F1 — itinerarios (2.1.1)
  // ======================================================

  /**
   * F1 – Encontrando itinerarios.
   *
   * Construye una función que, dados dos códigos de aeropuerto (cod1, cod2),
   * devuelve TODOS los itinerarios posibles (listas de vuelos) que van de
   * cod1 a cod2, sin repetir aeropuertos.
   *
   * Evidencia de programación funcional:
   *   - Recursión (función interna buscar → DFS).
   *   - Uso de conjunto inmutable visitados para evitar ciclos.
   *   - Expresión for (equivalente a filter + flatMap + map).
   *   - Reconocimiento de patrones a nivel de entrada/salida (case).
   */
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    // Preprocesamos los vuelos una sola vez: mapa origen -> lista de vuelos
    val porOrigen: Map[String, List[Vuelo]] = vuelosPorOrigen(vuelos)

    /**
     * Función recursiva que hace una búsqueda en profundidad (DFS)
     * para construir todos los itinerarios posibles.
     *
     * Parámetros:
     *   origen    → aeropuerto actual.
     *   destino   → aeropuerto destino final.
     *   visitados → conjunto de aeropuertos ya visitados (sin ciclos).
     *
     * Uso de programación funcional:
     *   - Recursión pura.
     *   - Conjuntos inmutables.
     *   - Expresión for para combinar vuelos y subrutas.
     */
    def buscar(origen: String, destino: String, visitados: Set[String]): List[Itinerario] =
      if (origen == destino)
        // Caso base: ya llegamos al destino → un único itinerario vacío.
        List(Nil)
      else {
        // Caso recursivo: exploramos todos los vuelos salientes del origen
        val vuelosSalientes = porOrigen.filter {case (k, _) => k == origen}.values.toList match { // Para filtrar mejor.
          case Nil => Nil
          case vs :: _ => vs
        }

        for {
          vuelo <- vuelosSalientes   // se actúa sobre lo filtrado anteriormente.
          if !visitados(vuelo.Dst)                    // evitamos regresar a aeropuertos ya visitados
          ruta  <- buscar(vuelo.Dst, destino, visitados + vuelo.Dst)
        } yield vuelo :: ruta
      } // construimos el itinerario agregando el vuelo actual

    /**
     * Función que el usuario realmente invoca:
     *   (cod1, cod2) => lista de itinerarios.
     *
     * Caso especial:
     *   - Si cod1 == cod2, retornamos List(Nil) (ya estamos en el destino).
     */
    (c1: String, c2: String) =>
      c1 match {
        case x if x == c2 => List(Nil)
        case _            => buscar(c1, c2, Set(c1))  // comenzamos con el origen marcado como visitado
      }
  }

  // ======================================================
  // F2 — itinerariosTiempo (2.1.2)
  // ======================================================

  /**
   * F2 – Minimización de tiempo total de viaje.
   *
   * Construye una función que, dados dos códigos de aeropuerto (cod1, cod2),
   * devuelve HASTA tres itinerarios que correspondan a los menores tiempos
   * totales de viaje (contando vuelo + esperas + zonas horarias).
   *
   * Evidencia de programación funcional:
   *   - Reutilización de F1 (composición de funciones).
   *   - Uso de map sobre mapas (pattern matching (c,a)).
   *   - Uso de sortBy como función de orden superior.
   *   - Uso de primerosN como recursión con patrones.
   */
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    // Mapa códigoAeropuerto -> GMT, para usar en tiempoTotal
    val gmtMap: Map[String, Int] =
      mapaAeropuertos(aeropuertos).map { case (c, a) => c -> a.GMT }

    // Reutilizamos la función F1 para generar todos los itinerarios posibles.
    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    (c1: String, c2: String) => {
      val its = todosItinerarios(c1, c2)
      // Ordenamos por tiempo total de viaje usando la función de alto orden sortBy
      val ordenados = its.sortBy(it => tiempoTotal(it, gmtMap))
      // Limitamos a 3 resultados usando un helper recursivo
      primerosN(3, ordenados)
    }
  }

  // ======================================================
  // F3 — itinerariosEscalas (2.1.3)
  // ======================================================

  /**
   * F3 – Minimización del número de escalas.
   *
   * Devuelve hasta 3 itinerarios entre cod1 y cod2 con menor cantidad
   * de escalas totales (cambios de avión + escalas técnicas).
   *
   * Criterios de orden:
   *   1) Menor número de escalasTotales.
   *   2) Menor número de vuelos (it.length).
   *   3) Menor tiempoTotal de viaje.
   *
   * Evidencia de programación funcional:
   *   - sortBy recibiendo una tupla (escalas, longitud, tiempo).
   *   - Uso de helpers escalasTotales y tiempoTotal.
   *   - Reutilización de itinerarios (F1).
   */
  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    // Mapa GMT para poder usar tiempoTotal como desempate
    val gmtMap: Map[String, Int] =
      aeropuertos.map(a => a.Cod -> a.GMT).toMap

    (c1: String, c2: String) => {
      val its = todosItinerarios(c1, c2)

      val ordenados =
        its.sortBy { it =>
          // Aquí usamos una tupla como clave de ordenamiento:
          // (escalaTotales, longitud, tiempoTotal)
          (
            escalasTotales(it),
            it.length,
            tiempoTotal(it, gmtMap)
          )
        }

      primerosN(3, ordenados)
    }
  }

  // ======================================================
  // F4 — itinerariosAire (2.1.4)
  // ======================================================

  /**
   * F4 – Minimización del tiempo en el aire.
   *
   * Devuelve hasta 3 itinerarios entre cod1 y cod2 que minimizan
   * la suma de distancias entre aeropuertos (tiempo en aire).
   *
   * Criterios de orden:
   *   1) Menor tiempoAireTotal (suma de distancias).
   *   2) Menor número de escalasTotales.
   *   3) Menor tiempoTotal de viaje.
   *
   * Evidencia de programación funcional:
   *   - sortBy sobre tuplas.
   *   - uso de helpers tiempoAireTotal, escalasTotales, tiempoTotal.
   *   - Composición de funciones, reutilizando itinerarios (F1).
   */
  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    val aeroMap = mapaAeropuertos(aeropuertos)
    val gmtMap: Map[String, Int] =
      aeropuertos.map(a => a.Cod -> a.GMT).toMap

    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    (c1: String, c2: String) => {
      val its = todosItinerarios(c1, c2)

      val ordenados =
        its.sortBy { it =>
          (
            tiempoAireTotal(it, aeroMap),      // criterio principal
            escalasTotales(it),               // menos escalas
            tiempoTotal(it, gmtMap)           // menor tiempo total
          )
        }

      primerosN(3, ordenados)
    }
  }

  // ======================================================
  // F5 — itinerarioSalida (2.1.5)
  // ======================================================

  /**
   * F5 – Optimización de la hora de salida.
   *
   * Construye una función que, dados:
   *   - cod1: aeropuerto de origen,
   *   - cod2: aeropuerto de destino,
   *   - hCita, mCita: hora local de la cita en el aeropuerto de destino,
   *
   * selecciona un itinerario que:
   *   1. Llega a cod2 en algún momento antes de la cita (se puede llegar días antes,
   *      siguiendo la interpretación del foro del profesor).
   *   2. Entre todos los que "sirven", tiene la hora de salida en cod1 lo más tarde posible.
   *
   * Evidencia de programación funcional:
   *   - Función interna con pattern matching (analizar).
   *   - Uso de listas inmutables para filtrar/transformar de forma declarativa.
   *   - Uso de maxBy como función de orden superior para seleccionar el mejor candidato.
   */
  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String, Int, Int) => Itinerario = {

    // Mapa código -> GMT para convertir horas locales a UTC
    val gmtMap: Map[String, Int] =
      aeropuertos.map(a => a.Cod -> a.GMT).toMap

    // Reutilizamos F1 para generar todos los itinerarios posibles
    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    (c1: String, c2: String, hCita: Int, mCita: Int) => {

      // Hora de la cita en UTC, usando el GMT del aeropuerto de destino
      val citaUTC = utcMinutes(hCita, mCita, gmtMap(c2))

      val its = todosItinerarios(c1, c2)

      /**
       * Función auxiliar que analiza un itinerario y, si es válido,
       * devuelve List((itinerario, depUTC)) donde depUTC es la hora
       * de salida (en UTC) del primer vuelo.
       *
       * Uso de pattern matching en listas:
       *   - case Nil
       *   - case list
       */
      def analizar(it: Itinerario): List[(Itinerario, Int)] = it match {
        case Nil =>
          // Itinerario vacío solo tiene sentido si origen y destino coinciden.
          if (c1 == c2) List(Nil -> citaUTC) else Nil

        case list =>
          val dep0    = utcMinutes(list.head.HS, list.head.MS, gmtMap(list.head.Org))
          val arr0Adj = adjust(dep0,
            utcMinutes(list.head.HL, list.head.ML, gmtMap(list.head.Dst)))

          // Reutilizamos la idea de tiempoTotal, pero aquí nos interesa
          // la hora de salida dep0 (para escoger la más tarde)
          val (_, lastArr) =
            list.tail.foldLeft((dep0, arr0Adj)) {
              case ((_, prevArr), v) =>
                val depAdj = adjust(prevArr,
                  utcMinutes(v.HS, v.MS, gmtMap(v.Org)))
                val arrAdj = adjust(depAdj,
                  utcMinutes(v.HL, v.ML, gmtMap(v.Dst)))
                (depAdj, arrAdj)
            }

          // Siguiendo el foro: cualquier itinerario que llega a cod2 se considera
          // aceptable (puede llegar días antes de la cita).
          List(it -> dep0)
      }

      // Usamos flatMap para:
      //   - aplicar 'analizar' a cada itinerario,
      //   - extraer directamente el par (itinerario, depUTC).
      val candidatos: List[(Itinerario, Int)] =
        its.flatMap(analizar)

      if (candidatos.isEmpty) Nil
      else {
        // Elegimos el itinerario cuya salida depUTC es la más tarde posible.
        val (mejorItinerario, _) =
          candidatos.maxBy { case (_, depUTC) => depUTC }
        mejorItinerario
      }
    }
  }
}
