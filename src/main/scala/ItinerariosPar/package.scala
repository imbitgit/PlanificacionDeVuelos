import Datos._
import common._
import Itinerarios._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

/**
 * Package object ItinerariosPar
 *
 * Contiene las versiones *paralelas* de:
 *   - F1: itinerariosPar        → encuentra todos los itinerarios entre dos aeropuertos.
 *   - F2: itinerariosTiempoPar  → selecciona hasta 3 itinerarios con menor tiempo total.
 *
 * La idea principal es reutilizar la lógica funcional de Itinerarios,
 * pero paralelizando los pasos intensivos usando colecciones paralelas (scala.collections.parallel).
 */
package object ItinerariosPar {

  // =========================
  // F1 paralelo: itinerariosPar
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
  // F2 paralelo: itinerariosTiempoPar
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
}
