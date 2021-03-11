package com.evolutiongaming.bootcamp.typeclass

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {
  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object GetSizeScore {
      def apply[T](implicit instance: GetSizeScore[T]): GetSizeScore[T] = instance
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore =  GetSizeScore[T].apply(inner)
        // def sizeScore: SizeScore =  implicitly[GetSizeScore[T]].apply(inner)
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values
      import syntax._
      import instances._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      def put(key: K, value: V): Unit = {
        val newElementScore = key.sizeScore + value.sizeScore
        if (newElementScore > maxSizeScore) return

        def helper(available: Int, required: Int): Unit = {
          if (available >= required) return
          popOldest().map { case (k, v) =>
            helper(available + k.sizeScore + v.sizeScore, required)
          }
        }

        helper(maxSizeScore - map.sizeScore + map.empty.sizeScore, newElementScore)
        map += (key -> value)
      }

      def get(key: K): Option[V] = map.get(key)

      private def popOldest(): Option[(K, V)] = {
        val oldest = map.headOption
        oldest.map { case (k, _) => map -= k }
        oldest
      }
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }
    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {
      import syntax._

      val OBJECT_HEADER_SCORE = 12

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val linkedHashMapIterate: Iterate2[mutable.LinkedHashMap] = new Iterate2[mutable.LinkedHashMap] {
        override def iterator1[T, S](f: mutable.LinkedHashMap[T, S]): Iterator[T] = f.keys.iterator
        override def iterator2[T, S](f: mutable.LinkedHashMap[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val packedMultiMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map(_._1).iterator
        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map(_._2).iterator
      }

      implicit val byteGetSizeScore: GetSizeScore[Byte] = (_: Byte) => 1

      implicit val charGetSizeScore: GetSizeScore[Char] = (_: Char) => 2

      implicit val intGetSizeScore: GetSizeScore[Int] = (_: Int) => 4

      implicit val longGetSizeScore: GetSizeScore[Long] = (_: Long) => 8

      implicit val stringGetSizeScore: GetSizeScore[String] =
        (s: String) => OBJECT_HEADER_SCORE +
          s.length * '_'.sizeScore

      implicit def iterableGetSizeScore[A[_]: Iterate, B: GetSizeScore]: GetSizeScore[A[B]] =
        iterable => OBJECT_HEADER_SCORE +
          implicitly[Iterate[A]].iterator(iterable).foldLeft(0) { (acc, v) => acc + v.sizeScore }

      implicit def Iterable2SizeScore[A[_, _]: Iterate2, T: GetSizeScore, S: GetSizeScore]: GetSizeScore[A[T, S]] =
        iterable => OBJECT_HEADER_SCORE +
          implicitly[Iterate2[A]].iterator1(iterable).foldLeft(0) { (acc, v) => acc + v.sizeScore } +
          implicitly[Iterate2[A]].iterator2(iterable).foldLeft(0) { (acc, v) => acc + v.sizeScore }
    }


  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._
    import syntax._
    import instances._

    final case class Twit(
      id: Long,
      userId: Int,
      hashTags: Vector[String],
      attributes: PackedMultiMap[String, String],
      fbiNotes: List[FbiNote],
    )

    implicit val twitGetSizeScore: GetSizeScore[Twit] =
      twit => OBJECT_HEADER_SCORE +
        twit.id.sizeScore +
        twit.userId.sizeScore +
        twit.hashTags.sizeScore +
        twit.attributes.sizeScore +
        twit.fbiNotes.sizeScore

    final case class FbiNote(
      month: String,
      favouriteChar: Char,
      watchedPewDiePieTimes: Long,
    )

    implicit val fbiNoteGetSizeScore: GetSizeScore[FbiNote] =
      note => OBJECT_HEADER_SCORE +
        note.month.sizeScore +
        note.favouriteChar.sizeScore +
        note.watchedPewDiePieTimes.sizeScore

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      val cache = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = cache.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = cache.get(id)
    }
  }
}
