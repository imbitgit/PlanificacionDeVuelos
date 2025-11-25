import Datos._
import common._
import Itinerarios._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

/**
 * Package object ItinerariosPar
 *
 * Contiene las versiones *paralelas* de:
 *   - (2.1.1) itinerarios        → itinerariosPar
 *   - (2.1.2) itinerariosTiempo  → itinerariosTiempoPar
 *   - (2.1.3) itinerariosEscalas → itinerariosEscalasPar
 *   - (2.1.4) itinerariosAire    → itinerariosAirePar
 *
 * La idea principal es reutilizar la lógica funcional de Itinerarios,
 * pero paralelizando los pasos intensivos usando colecciones paralelas (scala.collections.parallel).
 */
package object ItinerariosPar {

  // =========================
  // F1 paralelo: itinerariosPar (2.1.1)
  // =========================

  /**
   * F1 — Versión paralela de la búsqueda de itinerarios.
   *
   * Devuelve una función (cod1, cod2) => List[Itinerario] que genera
   * *todos* los itinerarios posibles sin ciclos, igual que en la versión secuencial,
   * pero paralelizando la exploración de los vuelos salientes desde cada aeropuerto.
   *
   * Estrategia:
   *   1. Agrupar los vuelos por aeropuerto de origen.
   *   2. Hacer una búsqueda en profundidad (DFS).
   *   3. Paralelizar la expansión de cada vuelo saliente mediante `.par.map`.
   *   4. Asegurar que no se repitan aeropuertos ya visitados.
   */
  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    // Estructura: origen -> lista de vuelos que salen de ese origen
    val porOrigen: Map[String, List[Vuelo]] = vuelos.groupBy(_.Org)

    /**
     * Función recursiva paralela.
     *
     * Parámetros:
     *   - origen: aeropuerto actual.
     *   - destino: aeropuerto final deseado.
     *   - visitados: registro de aeropuertos ya visitados (evita ciclos).
     *
     * En cada paso:
     *   - Obtenemos los vuelos salientes de "origen".
     *   - Filtramos los que causarían un ciclo.
     *   - Ejecutamos en paralelo cada rama de búsqueda usando `.par`.
     *   - Anteponemos el vuelo actual al itinerario recursivo.
     */
      def buscar(origen: String, destino: String, visitados: Set[String]): List[Itinerario] =
      if (origen == destino)
        // Caso base: ya estamos en destino → un único itinerario vacío.
        List(Nil)
      else {
        val salientes = porOrigen.getOrElse(origen, Nil)

        // PAR: procesamos todas las expansiones en paralelo.
        val resultados: ParSeq[List[Itinerario]] =
          salientes.par
            .filter(v => !visitados(v.Dst)) // evitamos ciclos
            .map { vuelo =>
              // Llamada recursiva en paralelo a cada destino alcanzable
              buscar(vuelo.Dst, destino, visitados + vuelo.Dst)
                // Anteponemos el vuelo actual
                .map(it => vuelo :: it)
            }

        // Aplanamos la estructura ParSeq[List[Itinerario]] en List[Itinerario].
        resultados.flatten.toList
      }

    /**
     * Devuelve la función solicitada:
     *   (c1, c2) => todos los itinerarios posibles.
     */
    (c1: String, c2: String) =>
      if (c1 == c2) List(Nil)
      else buscar(c1, c2, Set(c1))
  }

  // =========================
  // F2 paralelo: itinerariosTiempoPar (2.1.2)
  // =========================

  /**
   * F2 — Versión paralela del cálculo de los 3 itinerarios más rápidos.
   *
   * Devuelve una función (cod1, cod2) => List[Itinerario] que:
   *   1. Obtiene *todos* los itinerarios usando itinerariosPar.
   *   2. Calcula el tiempo total de cada itinerario *en paralelo*.
   *   3. Ordena por tiempo total.
   *   4. Devuelve los primeros 3.
   *
   * Uso de paralelismo:
   *   - `.par.map` para calcular el tiempo total de N itinerarios simultáneamente.
   */
  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    // Mapa código -> GMT para cálculos temporales
    val gmtMap: Map[String, Int] = aeropuertos.map(a => a.Cod -> a.GMT).toMap

    // Reutilizamos la versión paralela de F1
    val itsFuncPar = itinerariosPar(vuelos, aeropuertos)

    /**
     * Función resultante:
     *   (c1, c2) => 3 itinerarios más rápidos.
     */
    (c1: String, c2: String) => {
      val its = itsFuncPar(c1, c2)  // todos los itinerarios en paralelo

      // PAR: calcular el tiempo total de todos los itinerarios simultáneamente
      val calificados: ParSeq[(Itinerario, Int)] =
        its.par.map(it => (it, tiempoTotal(it, gmtMap)))

      // Convertimos a lista normal, ordenamos por tiempo y tomamos 3
      calificados.toList
        .sortBy(_._2)
        .map(_._1)
        .take(3)
    }
  }

  // ============================================================
  // F3 paralelo: itinerariosEscalasPar (2.1.3)
  // ============================================================

  /**
   * F3 — Versión paralela de la minimización de escalas.
   *
   * Devuelve una función (cod1, cod2) ⇒ List[Itinerario] que:
   *   1. Obtiene todos los itinerarios mediante itinerariosPar.
   *   2. Calcula en paralelo el número total de escalas de cada itinerario
   *   usando escalasTotales (definida en Itinerarios).
   *   3. Ordena por número de escalas ascendentes.
   *   4. Devuelve hasta los primeros 3.
   *
   * El cálculo de escalas es independiente por itinerario,
   * por lo cual paralelizarlo es seguro y eficiente.
   */
  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    val itsFuncPar = itinerariosPar(vuelos, aeropuertos)

    (c1: String, c2: String) => {
      val its = itsFuncPar(c1, c2)

      // PAR: calcular escalas cada itinerario simultáneamente
      val calificados: ParSeq[(Itinerario, Int)] =
        its.par.map(it => (it, escalasTotales(it)))

      calificados.toList
        .sortBy(_._2)
        .map(_._1)
        .take(3)
    }
  }

  // ============================================================
  // F4 paralelo: itinerariosAirePar (2.1.4)
  // ============================================================

  /**
   * F4 — Versión paralela de la minimización del tiempo en el aire.
   *
   * Devuelve una función (cod1, cod2) ⇒ List[Itinerario] que:
   *   1. Genera todos los itinerarios usando itinerariosPar.
   *   2. Calcula en paralelo el "tiempo en el aire" mediante tiempoAireTotal.
   *   3. Ordena crecientemente dichas distancias acumuladas.
   *   4. Devuelve los 3 mejores.
   *
   * Este cálculo es totalmente independiente por itinerario,
   * por lo que se paraleliza con `.par.map`.
   */
  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String) => List[Itinerario] = {

    val aeroMap = aeropuertos.map(a => a.Cod -> a).toMap
    val itsFuncPar = itinerariosPar(vuelos, aeropuertos)

    (c1: String, c2: String) => {
      val its = itsFuncPar(c1, c2)

      // PAR: cálculo simultáneo de tiempo en aire
      val calificados: ParSeq[(Itinerario, Double)] =
        its.par.map(it => (it, tiempoAireTotal(it, aeroMap)))

      calificados.toList
        .sortBy(_._2)
        .map(_._1)
        .take(3)
    }
  }

  // ============================================================
  // F5 paralelo: itinerarioSalidaPar (2.1.5)
  // ============================================================

  /**
   * F5 — Versión paralela de la optimización de la hora de salida.
   *
   * Construye una función (cod1, cod2, hCita, mCita) ⇒ Itinerario que:
   *   1. Obtiene todos los itinerarios posibles mediante itinerariosPar (ya paralela).
   *      2. Para cada itinerario, calcula EN PARALELO:
   *        - la hora de salida en UTC,
   *        - la hora de llegada total en UTC (misma lógica que tiempoTotal).
   *          3. Filtra aquellos que llegan ANTES o EXACTAMENTE a la hora de la cita.
   *          4. Entre ellos, selecciona el itinerario cuya salida sea lo más tarde posible.
   *
   * Si ningún itinerario llega a tiempo, devuelve Nil.
   */
  def itinerarioSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
  : (String, String, Int, Int) => Itinerario = {

    // Necesitamos GMT por aeropuerto para convertir a UTC
    val gmtMap: Map[String, Int] =
      aeropuertos.map(a => a.Cod -> a.GMT).toMap

    // Reutilizamos F1 versión paralela
    val itsPar = itinerariosPar(vuelos, aeropuertos)

    (c1: String, c2: String, hCita: Int, mCita: Int) => {

      // Convertimos la hora local de la cita en cod2 → UTC
      val citaUTC = utcMinutes(hCita, mCita, gmtMap(c2))

      // Todos los itinerarios en paralelo
      val its = itsPar(c1, c2)

      // Evaluamos en paralelo salida/llegada para cada itinerario
      val candidatosPar: ParSeq[(Itinerario, Int)] =
        its.par.flatMap {
          case Nil =>
            // Caso especial: origen y destino iguales
            if (c1 == c2) ParSeq((Nil, citaUTC))
            else ParSeq.empty

          case first :: rest =>
            // === MISMA LÓGICA QUE tiempoTotal + guardar dep0 ===
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
            // ================================================

            // Solo tomamos itinerarios que llegan a tiempo
            if (lastArr <= citaUTC)
              ParSeq((first :: rest, dep0))
            else
              ParSeq.empty
        }

      val candidatos = candidatosPar.toList

      // Si no hay itinerarios viables → Nil
      if (candidatos.isEmpty) Nil
      else {
        // Elegimos el que sale LO MÁS TARDE (max depUTC)
        val (mejor, _) =
          candidatos.maxBy { case (_, depUTC) => depUTC }
        mejor
      }
    }
  }
}

