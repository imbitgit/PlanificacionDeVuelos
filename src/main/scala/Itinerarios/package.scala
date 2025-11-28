import Datos._

/**
 * Package object Itinerarios
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

  // =========================
  // Helpers comunes
  // =========================

  /**
   * Construye un mapa códigoAeropuerto -> Aeropuerto.
   *
   * Esto permite acceder en O(1) al aeropuerto a partir de su código,
   * en lugar de recorrer la lista cada vez.
   * Se usa en (2.1.2) itinerariosTiempo -> para obtener GMT
   * Se usa en (2.1.4) itinerariosAire -> para obtener coordenadas X,Y
   * Se usa en (2.1.5) itinerarioSalida  -> para convertir horas a UTC.
   */
  private def mapaAeropuertos(aeropuertos: List[Aeropuerto]): Map[String, Aeropuerto] =
    aeropuertos.map(a => a.Cod -> a).toMap

  /**
   * Agrupa los vuelos por aeropuerto de origen.
   *
   * Resultado: Map(org -> List(vuelos que salen de org)).
   * Nos sirve para, dado un aeropuerto, obtener rápidamente
   * todos los vuelos que salen de él.
   * Se usa en (2.1.2) itinerariosTiempo -> busqueda todos los itinerarios
   * Se usa en (2.1.2-2.1.5) reutilizan itenerarios
   */
  private def vuelosPorOrigen(vuelos: List[Vuelo]): Map[String, List[Vuelo]] =
    vuelos.groupBy(_.Org)

  /**
   * Convierte una hora local (h:m) a minutos UTC, usando el GMT del aeropuerto.
   * Se usa en (2.1.2) tiempoTotal/ itinerariosTiempo
   * Se usa en (2.1.5) itinerarioSalida (cálculo con hora de cita).
   *
   * Ejemplos:
   *   gmt = -500 → offsetHoras = -5
   *   gmt =  300 → offsetHoras =  3
   *
   * La relación es:
   *   horaLocal = horaUTC + offset
   *   ⇒ horaUTC = horaLocal - offset
   */
  def utcMinutes(h: Int, m: Int, gmt: Int): Int = {
    val offsetHours = gmt / 100             // -500 → -5, 300 → 3
    h * 60 + m - offsetHours * 60           // pasamos todo a minutos
  }
  /** Minutos que tiene un día completo (24 horas). */
  val DayMinutes: Int = 24 * 60


  /**
   * Ajusta un tiempo t para que sea mayor o igual que "after",
   * sumando días completos (de 24h) si es necesario.
   * Se usa en (2.1.2) tiempoTotal/ itinerariosTiempo
   * Se usa en (2.1.5) itinerarioSalida (para cadenas de vuelos).
   * Esto se usa para:
   *   - Asegurar que la hora de llegada no sea anterior a la de salida.
   *   - Manejar vuelos que cruzan medianoche o duran más de un día.
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
   * Se usa en:
   *   - (2.1.2) itinerariosTiempo
   *   - (2.1.3) itinerariosEscalas
   *   - (2.1.4) itinerariosAire
   *     para limitar la respuesta a lo pedido en el enunciado (máximo 3 itinerarios).
   */
  def primerosN[A](n: Int, xs: List[A]): List[A] = (n, xs) match {
    case (_, Nil) => Nil // lista vacía -> nada que devolver
    case (0, _) => Nil // ya tomamos n elementos
    case (k, h :: t) => h :: primerosN(k - 1, t)
  }


  /**
   *Número total de escalas de un itinerario.
   * Se usa en (2.1.3) Número total de escalas de un itinerario.
   * Según el enunciado, hay dos tipos de escalas:
   *   - Cambio explícito de vuelo (de un Vuelo al siguiente en la lista).
   *   - Escalas técnicas dentro de un vuelo (campo Esc de Vuelo).
   *
   * Por eso:
   * escalasTotales = (número de cambios de avión) + sum(Esc de cada vuelo)
   * = (it.length - 1) + it.map(_.Esc).sum, si no es vacío.
   */
  
   def escalasTotales(it: Itinerario): Int = it match {
     case Nil => 0
     case _=> (it.length - 1) + it.map(_.Esc).sum
   }





  // Se usa en (2.1.4) Distancia geométrica entre dos aeropuertos.
  // Usada por tiempoAireTotal para calcular el "tiempo en el aire".
  def distancia(a1: Aeropuerto, a2: Aeropuerto): Double = {
    val dx = a1.X - a2.X
    val dy = a1.Y - a2.Y
    math.sqrt(dx.toDouble * dx.toDouble + dy.toDouble * dy.toDouble)
  }

  /**
   * Se usa en (2.1.4) Tiempo total "en el aire" para un itinerario.
   *
   * Modela el tiempo de vuelo como la suma de distancias entre:
   * origen(vuelo) → destino(vuelo) para cada vuelo.
   *
   * Lo usan:
   *   - itinerariosAire    (2.1.4, secuencial).
   *   - itinerariosAirePar (2.1.4, paralelo).
   */
  def tiempoAireTotal(it: Itinerario, aeroMap: Map[String, Aeropuerto]): Double =
    it.foldLeft(0.0) { (acum, v) =>
      val org = aeroMap(v.Org)
      val dst = aeroMap(v.Dst)
      acum + distancia(org, dst)
    }


  /**
   * Calcula el tiempo total de viaje de un itinerario en minutos.
   * Se usa en (2.1.2) tiempo total de viaje de un itinerario en minutos
   *
   * El tiempo total incluye:
   *   - Tiempo de vuelo.
   *   - Tiempos de espera entre conexiones.
   *   - Diferencias de huso horario (GMT) entre aeropuertos.
   *
   * Recibe:
   *   it     → lista de vuelos (Itinerario).
   *   gmtMap → mapa códigoAeropuerto -> GMT.
   *
   * Estrategia:
   *   - Caso base: itinerario vacío → 0.
   *   - Para el primer vuelo: calculamos salida y llegada en UTC.
   *   - Para el resto, usamos foldLeft acumulando:
   *       (últimaSalidaAjustada, últimaLlegadaAjustada)
   *     y garantizando que cada vuelo sale después de la llegada anterior.
   *   - Resultado final: últimaLlegada - primeraSalida.
   */
  def tiempoTotal(it: Itinerario, gmtMap: Map[String, Int]): Int = it match {
    case Nil => 0
    case first :: rest =>
      // Conversión de la primera salida y llegada a UTC
      val dep0     = utcMinutes(first.HS, first.MS, gmtMap(first.Org))
      val arr0Raw  = utcMinutes(first.HL, first.ML, gmtMap(first.Dst))
      val arr0Adj  = adjust(dep0, arr0Raw) // llegada nunca antes de la salida

      // Recorremos el resto de vuelos actualizando salida/llegada
      val (_, lastArr) = rest.foldLeft((dep0, arr0Adj)) {
        case ((_, prevArr), vuelo) =>
          // Hora de salida en UTC
          val depRaw = utcMinutes(vuelo.HS, vuelo.MS, gmtMap(vuelo.Org))
          // Ajustamos para que sea >= llegada anterior
          val depAdj = adjust(prevArr, depRaw)
          // Hora de llegada en UTC y ajustada
          val arrRaw = utcMinutes(vuelo.HL, vuelo.ML, gmtMap(vuelo.Dst))
          val arrAdj = adjust(depAdj, arrRaw)
          (depAdj, arrAdj)
      }

      // Tiempo total = última llegada - primera salida
      lastArr - dep0
  }

  // =========================
  // F1: itinerarios
  // =========================

  /**
   * F1 – Encontrando itinerarios.
   *
   * Construye una función que, dados dos códigos de aeropuerto (cod1, cod2),
   * devuelve TODOS los itinerarios posibles (listas de vuelos) que van de
   * cod1 a cod2, sin repetir aeropuertos.
   *
   * Uso típico:
   *   val its = itinerarios(vuelosCurso, aeropuertosCurso)
   *   val rutas = its("CLO", "SVO")
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
     *   visitados → conjunto de aeropuertos que ya hemos visitado
     *               en el camino actual (para evitar ciclos).
     *
     * Resultado:
     *   Lista de itinerarios (List[Itinerario]), donde cada itinerario
     *   es una lista de vuelos desde "origen" hasta "destino".
     */
    def buscar(origen: String, destino: String, visitados: Set[String]): List[Itinerario] =
      if (origen == destino) {
        // Caso base: ya llegamos al destino.
        // Representamos “camino vacío” con Nil.
        List(Nil)
      } else {
        // Vuelos que salen del aeropuerto actual
        val salientes = porOrigen.getOrElse(origen, Nil)

        salientes
          // No volvemos a un aeropuerto ya visitado (evitamos ciclos)
          .filter(v => !visitados(v.Dst))
          // Para cada vuelo, extendemos los itinerarios desde su destino
          .flatMap { vuelo =>
            buscar(vuelo.Dst, destino, visitados + vuelo.Dst)
              // Anteponemos el vuelo actual al itinerario recursivo
              .map(it => vuelo :: it)
          }
      }

    /**
     * Función que el usuario realmente invoca:
     *   (cod1, cod2) => lista de itinerarios.
     *
     * Caso especial: si cod1 == cod2, por convenio devolvemos List(Nil),
     * interpretado como “itinerario vacío” (ya estamos en el destino).
     */
    (c1: String, c2: String) =>
      if (c1 == c2) List(Nil)
      else buscar(c1, c2, Set(c1))  // empezamos habiendo visitado el origen
  }

  // =========================
  // F2: itinerariosTiempo
  // =========================

  /**
   * F2 – Minimización de tiempo total de viaje.
   *
   * Construye una función que, dados dos códigos de aeropuerto (cod1, cod2),
   * devuelve HASTA tres itinerarios que correspondan a los menores tiempos
   * totales de viaje (contando vuelo + esperas + zonas horarias).
   *
   * Si hay menos de 3 itinerarios posibles, devuelve todos los que existan.
   */
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    // Mapa códigoAeropuerto -> GMT, para usar en tiempoTotal
    val gmtMap: Map[String, Int] =
      mapaAeropuertos(aeropuertos).view.mapValues(_.GMT).toMap

    // Reutilizamos la función F1 para generar todos los itinerarios posibles.
    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    /**
     * Función que el usuario invoca:
     *   (cod1, cod2) => lista de los mejores itinerarios en tiempo.
     *
     * Estrategia:
     *   1. Obtenemos todos los itinerarios cod1 → cod2.
     *   2. Calculamos su tiempo total usando tiempoTotal.
     *   3. Ordenamos de menor a mayor tiempo.
     *   4. Tomamos como máximo los primeros 3.
     */
    (c1: String, c2: String) => {
      val its = todosItinerarios(c1, c2)               // todos los itinerarios posibles
      val ordenados = its.sortBy(it => tiempoTotal(it, gmtMap))
      primerosN(3, ordenados)
    }
  }

  // =========================
  // F3: itinerariosEscalas (2.1.3)
  // =========================

  // =========================
  // F3: itinerariosEscalas (2.1.3)
  // =========================

  /**
   * F3 - Minimización del número de escalas.
   *
   * Devuelve hasta 3 itinerarios entre cod1 y cod2 con menor cantidad
   * de escalas totales (cambios de avión + escalas técnicas).
   * En caso de empate:
   *   - se prefieren menos vuelos en el itinerario,
   *   - y luego menor tiempo total de viaje.
   */
  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    // Mapa GMT para poder usar tiempoTotal como desempate
    val gmtMap: Map[String, Int] =
      aeropuertos.map(a => a.Cod -> a.GMT).toMap

    (c1: String, c2: String) => {
      val its = todosItinerarios(c1, c2)

      // Ordenamos por:
      // 1) menos escalasTotales
      // 2) menos vuelos (it.length)
      // 3) menor tiempoTotal
      val ordenados = its.sortBy { it =>
        (
          escalasTotales(it),
          it.length,
          tiempoTotal(it, gmtMap)
        )
      }

      primerosN(3, ordenados)
    }
  }


  // =========================
  // F4: itinerariosAire (2.1.4)
  // =========================

  /**
   * F4 - Minimización del tiempo en el aire.
   *
   * Devuelve hasta 3 itinerarios entre cod1 y cod2 que minimizan
   * la suma de distancias entre aeropuertos (tiempo en aire).
   * En caso de empate:
   *   - se prefieren menos escalas,
   *   - y luego menor tiempo total de viaje.
   */
  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    val aeroMap = mapaAeropuertos(aeropuertos)
    val gmtMap: Map[String, Int] =
      aeropuertos.map(a => a.Cod -> a.GMT).toMap

    val todosItinerarios = itinerarios(vuelos, aeropuertos)

    (c1: String, c2: String) => {
      val its = todosItinerarios(c1, c2)

      // Ordenamos por:
      // 1) tiempo en aire (distancia total)
      // 2) número de escalas
      // 3) tiempo total de viaje
      val ordenados = its.sortBy { it =>
        (
          tiempoAireTotal(it, aeroMap), // criterio principal
          escalasTotales(it), // desempate 1: menos escalas
          tiempoTotal(it, gmtMap) // desempate 2: menor tiempo total
        )
      }
      primerosN(3, ordenados)
    }
  }


  // =========================
  // F5: itinerarioSalida (2.1.5)
  // =========================

  /**
   * F5 – Optimización de la hora de salida.
   *
   * Construye una función que, dados:
   *   - cod1: aeropuerto de origen,
   *   - cod2: aeropuerto de destino,
   *   - hCita, mCita: hora local de la cita en el aeropuerto de destino,
   *
   * selecciona un itinerario que:
   *   1. Llega a cod2 **a tiempo o antes** de la cita.
   *      2. Entre todos los que llegan a tiempo, tiene la hora de salida
   *      en cod1 lo más tarde posible.
   *
   * Si ningún itinerario llega a tiempo, devuelve el itinerario vacío (Nil).
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

      // Para cada itinerario calculamos salida y llegada en UTC
      // (misma lógica de tiempoTotal), pero **NO** filtramos por la hora de la cita,
      // siguiendo la interpretación del profesor: se puede llegar el día anterior.
      val candidatos: List[(Itinerario, Int)] =
        its.flatMap {
          case Nil =>
            // Itinerario vacío: solo tiene sentido cuando c1 == c2.
            if (c1 == c2) List((Nil, citaUTC)) else Nil

          case first :: rest =>
            val dep0 = utcMinutes(first.HS, first.MS, gmtMap(first.Org))
            val arr0Raw = utcMinutes(first.HL, first.ML, gmtMap(first.Dst))
            val arr0Adj = adjust(dep0, arr0Raw)

            val (_, lastArr) = rest.foldLeft((dep0, arr0Adj)) {
              case ((_, prevArr), vuelo) =>
                val depRaw = utcMinutes(vuelo.HS, vuelo.MS, gmtMap(vuelo.Org))
                val depAdj = adjust(prevArr, depRaw)
                val arrRaw = utcMinutes(vuelo.HL, vuelo.ML, gmtMap(vuelo.Dst))
                val arrAdj = adjust(depAdj, arrRaw)
                (depAdj, arrAdj)
            }

            // Siguiendo el foro: cualquier itinerario que llega a cod2
            // se puede interpretar como que llega algún día antes de la cita.
            // Por tanto, TODOS son candidatos; la cita solo se usa para
            // decidir cuál salida queremos (la más tarde posible).
            List((first :: rest, dep0))
        }


      if (candidatos.isEmpty) Nil
      else {
        // Elegimos el itinerario que **más tarde** sale (mayor depUTC)
        val (mejorItinerario, _) =
          candidatos.maxBy { case (_, depUTC) => depUTC }
        mejorItinerario
      }
    }
  }
}


