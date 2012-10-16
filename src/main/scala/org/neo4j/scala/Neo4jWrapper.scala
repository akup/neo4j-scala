/*
 *                                The MIT License

Copyright (c) 2009, Jawher Moussa and Martin Kleppmann
Modified work Copyright 2012 Alexander Kuprin  


Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

 */

/*
 * Originaly published by Jawher Moussa at https://github.com/jawher/neo4j-scala
 * Modified by Alexander Kuprin and published at https://github.com/akup/neo4j-scala
 */

package org.neo4j.scala

import org.neo4j.graphdb._
import org.neo4j.graphdb.index._
import collection.JavaConversions._
import collection.{generic, SetLike, immutable, mutable}
import mutable.{SetBuilder, Builder}

/**
 * Extend your class with this trait to get really neat new notation for creating
 * new relationships. For example, ugly Java-esque code like:
 * <pre>
 * val knows = DynamicRelationshipType.withName("KNOWS")
 * start.createRelationshipTo(intermediary, knows)
 * intermediary.createRelationshipTo(end, knows)
 * </pre>
 *
 * can be replaced with a beautiful Scala one-liner:
 * <pre>start --> "KNOWS" --> intermediary --> "KNOWS" --> end</pre>
 *
 * Feel free to use this example to tell all your friends how awesome scala is :)
 */
trait Neo4jWrapper {

    /**
   * Execute instructions within a Neo4j transaction; rollback if exception is raised and
   * commit otherwise; and return the return value from the operation.
   */
  def execInNeo4j[T<:Any](operation: GraphDatabaseService => T)(implicit neo : GraphDatabaseService): T = {
    val tx = synchronized {
      neo.beginTx
    }
    try {
      val ret = operation(neo)
      tx.success
      return ret
    } finally {
      tx.finish
    }
  }
  
  def execInNeo4jWriteLock[T<:Any](lock_inst: PropertyContainer, operation: GraphDatabaseService => T)(implicit neo : GraphDatabaseService): T = {
    val tx = synchronized {
      neo.beginTx
    }
    try {
      tx.acquireWriteLock(lock_inst)
      val ret = operation(neo)
      tx.success
      return ret
    } finally {
      tx.finish
    }
  }
  
  def execInNeo4jReadLock[T<:Any](lock_inst: PropertyContainer, operation: GraphDatabaseService => T)(implicit neo : GraphDatabaseService): T = {
    val tx = synchronized {
      neo.beginTx
    }
    try {
      tx.acquireReadLock(lock_inst)
      val ret = operation(neo)
      tx.success
      return ret
    } finally {
      tx.finish
    }
  }

  class NodeRelationshipMethods(node: Node, _rel: Option[Relationship] = None) {

    def -->(relType: RelationshipType) = new OutgoingRelationshipBuilder(node, relType)

    // Create incoming relationship

    def <--(relType: RelationshipType) = new IncomingRelationshipBuilder(node, relType)

    def rel = _rel
  }

  // Half-way through building an outgoing relationship
  class OutgoingRelationshipBuilder(fromNode: Node, relType: RelationshipType) {
    def -->(toNode: Node) = {
      val rel = fromNode.createRelationshipTo(toNode, relType)
      new NodeRelationshipMethods(toNode, Some(rel))
    }
  }

  // Half-way through building an incoming relationship
  class IncomingRelationshipBuilder(toNode: Node, relType: RelationshipType) {
    def <--(fromNode: Node) = {
      val rel = fromNode.createRelationshipTo(toNode, relType)
      new NodeRelationshipMethods(fromNode, Some(rel))
    }
  }

  implicit def node2relationshipBuilder(node: Node) = new NodeRelationshipMethods(node)

  implicit def string2RelationshipType(relType: String) = DynamicRelationshipType.withName(relType)
  
  class OptionalNodeRelationshipMethods(node: Option[Node], _rel: Option[Relationship] = None) {

    def -->(relType: RelationshipType) = new OptionalOutgoingRelationshipBuilder(node, relType)

    // Create incoming relationship

    def <--(relType: RelationshipType) = new OptionalIncomingRelationshipBuilder(node, relType)

    def rel = _rel
  }

  // Half-way through building an outgoing relationship
  class OptionalOutgoingRelationshipBuilder(fromNode: Option[Node], relType: RelationshipType) {
    def -->(toNode: Node): OptionalNodeRelationshipMethods = this.-->(Some(toNode))
    
    def -->(toNode: Option[Node]): OptionalNodeRelationshipMethods = {
      val rel = toNode match {
        case Some(toN) => fromNode.map(_.createRelationshipTo(toN, relType))
        case _ => None
      }
      new OptionalNodeRelationshipMethods(toNode, rel)
    }
  }

  // Half-way through building an incoming relationship
  class OptionalIncomingRelationshipBuilder(toNode: Option[Node], relType: RelationshipType) {
    def <--(fromNode: Node): OptionalNodeRelationshipMethods = this.<--(Some(fromNode))
    
    def <--(fromNode: Option[Node]): OptionalNodeRelationshipMethods = {
      val rel = toNode match {
        case Some(toN) => fromNode.map(_.createRelationshipTo(toN, relType))
        case _ => None
      }
      new OptionalNodeRelationshipMethods(toNode, rel)
    }
  }

  implicit def optionalNode2relationshipBuilder(node: Option[Node]) = new OptionalNodeRelationshipMethods(node)

  class RichPropertyContainer(propertyContainer: PropertyContainer) {
    def apply(property: String) : Option[Any] = if(propertyContainer.hasProperty(property)) Some(propertyContainer.getProperty(property)) else None
    def update(property: String, value: Any) : Unit = propertyContainer.setProperty(property, value)
    def delete(property: String): Unit = propertyContainer.removeProperty(property)
  }

  implicit def propertyContainer2RichPropertyContainer(propertyContainer: PropertyContainer) = new RichPropertyContainer(propertyContainer)

  implicit def fn2StopEvaluator(e : TraversalPosition => Boolean) = 
    new StopEvaluator() {
      def isStopNode(traversalPosition : TraversalPosition) = e(traversalPosition)
    }

  implicit def fn2ReturnableEvaluator(e : TraversalPosition => Boolean) = 
    new ReturnableEvaluator () {
      def isReturnableNode(traversalPosition : TraversalPosition) = e(traversalPosition)
    }

  class RichNode(node: Node) {
    def rels = node.getRelationships
    def rels(dir: Direction) = node.getRelationships(dir)
    def rels(types: RelationshipType) = node.getRelationships(types)
    def rels(t: RelationshipType, dir: Direction) = node.getRelationships(t, dir)
  }

  implicit def node2RichNode(node: Node) = new RichNode(node)

  implicit def indexHitsToList[T](indHits: IndexHits[T]): List[T] = {
    var list = List[T]()
    for (node <- asScalaIterator(indHits)) list ::= node
    list
  }

  /**
    *    Clone of Enumeration
  */
abstract class RelEnum(initial: Int, names: String*) extends Serializable {
  thisenum =>

  def this() = this(0)
  def this(names: String*) = this(0, names: _*)

  /* Note that `readResolve` cannot be private, since otherwise
     the JVM does not invoke it when deserializing subclasses. */
  protected def readResolve(): AnyRef = thisenum.getClass.getField("MODULE$").get()

  /** The name of this enumeration.
   */
  override def toString = (getClass.getName stripSuffix "$" split '.' last) split '$' last

  /** The mapping from the integer used to identify values to the actual
    * values. */
  private val vmap: mutable.Map[Int, Value] = new mutable.HashMap

  /** The cache listing all values of this enumeration. */
  @transient private var vset: ValueSet = null
  @transient private var vsetDefined = false

  /** The mapping from the integer used to identify values to their
    * names. */
  private val nmap: mutable.Map[Int, String] = new mutable.HashMap

  /** The values of this enumeration as a set.
   */
  def values: ValueSet = {
    if (!vsetDefined) {
      vset = new ValueSet(immutable.SortedSet.empty[Int] ++ (vmap.values map (_.id)))
      vsetDefined = true
    }
    vset
  }

  /** The integer to use to identify the next created value. */
  protected var nextId = initial

  /** The string to use to name the next created value. */
  protected var nextName = names.iterator
  private def nextNameOrNull =
    if (nextName.hasNext) nextName.next else null

  /** The highest integer amongst those used to identify values in this
    * enumeration. */
  private var topId = initial

  /** The highest integer amongst those used to identify values in this
    * enumeration. */
  final def maxId = topId

  /** The value of this enumeration with given id `x`
   */
  final def apply(x: Int): Value = vmap(x)

  /** Returns a Value from this Enumeration whose name matches
   * the argument <var>s</var>.
   *
   * You can pass a String* set of names to the constructor, or
   * initialize each Enumeration with Value(String). Otherwise, the
   * names are determined automatically through reflection.
   *
   * Note the change here wrt 2.7 is intentional. You should know whether
   * a name is in an Enumeration beforehand. If not, just use find on
   * values.
   *
   * @param  s an Enumeration name
   * @return   the Value of this Enumeration if its name matches <var>s</var>
   * @throws   java.util.NoSuchElementException if no Value with a matching
   *           name is in this Enumeration
   */
  final def withName(s: String): Value = values.find(_.toString == s).get

  /** Creates a fresh value, part of this enumeration. */
  protected final def Value: Value = Value(nextId)

  /** Creates a fresh value, part of this enumeration, identified by the integer
   *  `i`.
   *
   *  @param i An integer that identifies this value at run-time. It must be
   *           unique amongst all values of the enumeration.
   *  @return  Fresh value identified by `i`.
   */
  protected final def Value(i: Int): Value = Value(i, nextNameOrNull)

  /** Creates a fresh value, part of this enumeration, called `name`.
   *
   *  @param name A human-readable name for that value.
   *  @return  Fresh value called `name`.
   */
  protected final def Value(name: String): Value = Value(nextId, name)

  /** Creates a fresh value, part of this enumeration, called `name`
   *  and identified by the integer `i`.
   *
   * @param i    An integer that identifies this value at run-time. It must be
   *             unique amongst all values of the enumeration.
   * @param name A human-readable name for that value.
   * @return     Fresh value with the provided identifier `i` and name `name`.
   */
  protected final def Value(i: Int, name: String): Value = new Val(i, name)

  private def populateNameMap() {
    // The list of possible Value methods: 0-args which return a conforming type
    val methods = getClass.getMethods filter (m => m.getParameterTypes.isEmpty &&
                                                   classOf[Value].isAssignableFrom(m.getReturnType) &&
                                                   m.getDeclaringClass != classOf[Enumeration])
    methods foreach { m =>
      val name = m.getName
      // invoke method to obtain actual `Value` instance
      val value = m.invoke(this).asInstanceOf[Value]
      // verify that outer points to the correct Enumeration: ticket #3616.
      if (value.outerEnum eq thisenum) {
        val id = Int.unbox(classOf[Val] getMethod "id" invoke value)
        nmap += ((id, name))
      }
    }
  }

  /* Obtains the name for the value with id `i`. If no name is cached
   * in `nmap`, it populates `nmap` using reflection.
   */
  private def nameOf(i: Int): String = synchronized { nmap.getOrElse(i, { populateNameMap() ; nmap(i) }) }

  /** The type of the enumerated values. */
  @SerialVersionUID(7091335633555234129L)
  abstract class Value extends Ordered[Value] with Serializable with RelationshipType {
    /** the id and bit location of this enumeration value */
    def id: Int
    /** a marker so we can tell whose values belong to whom come reflective-naming time */
    private[RelEnum] val outerEnum = thisenum

    override def compare(that: Value): Int = this.id - that.id
    override def equals(other: Any) = other match {
      case that: RelEnum#Value  => (outerEnum eq that.outerEnum) && (id == that.id)
      case _                        => false
    }
    override def hashCode: Int = id.##

    override def name = toString
  }

  /** A class implementing the <a href="Enumeration.Value.html"
   *  target="contentFrame">`Value`</a> type. This class can be
   *  overridden to change the enumeration's naming and integer identification
   *  behaviour.
   */
  @SerialVersionUID(0 - 3501153230598116017L)
  protected class Val(i: Int, name: String) extends Value with Serializable {
    def this(i: Int)        = this(i, nextNameOrNull)
    def this(name: String)  = this(nextId, name)
    def this()              = this(nextId)

    assert(!vmap.isDefinedAt(i), "Duplicate id: " + i)
    vmap(i) = this
    vsetDefined = false
    nextId = i + 1
    if (nextId > topId) topId = nextId
    def id = i
    override def toString() =
      if (name != null) name
      else try thisenum.nameOf(i)
      catch { case _: NoSuchElementException => "<Invalid enum: no field for #" + i + ">" }

    protected def readResolve(): AnyRef = {
      val enum = thisenum.readResolve().asInstanceOf[RelEnum]
      if (enum.vmap == null) this
      else enum.vmap(i)
    }
  }

  /** A class for sets of values
   *  Iterating through this set will yield values in increasing order of their ids.
   *  @param   ids   The set of ids of values, organized as a SortedSet.
   */
  class ValueSet private[RelEnum] (val ids: immutable.SortedSet[Int]) extends Set[Value] with SetLike[Value, ValueSet] {
    override def empty = ValueSet.empty
    def contains(v: Value) = ids contains (v.id)
    def + (value: Value) = new ValueSet(ids + value.id)
    def - (value: Value) = new ValueSet(ids - value.id)
    def iterator = ids.iterator map thisenum.apply
    override def stringPrefix = thisenum + ".ValueSet"
  }

  /** A factory object for value sets */
  object ValueSet {
    import mutable.{ Builder, SetBuilder }
    import generic.CanBuildFrom

    /** The empty value set */
    val empty = new ValueSet(immutable.SortedSet.empty)
    /** A value set consisting of given elements */
    def apply(elems: Value*): ValueSet = empty ++ elems
    /** A builder object for value sets */
    def newBuilder: Builder[Value, ValueSet] = new SetBuilder(empty)
    /** The implicit builder for value sets */
    implicit def canBuildFrom: CanBuildFrom[ValueSet, Value, ValueSet] =
      new CanBuildFrom[ValueSet, Value, ValueSet] {
        def apply(from: ValueSet) = newBuilder
        def apply() = newBuilder
      }
  }
}
}

object Neo4jImplicits extends Neo4jWrapper {}
