/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.uml.canonicalXMI

import java.io.Serializable
import java.lang.System

import org.omg.oti.json.common.OTIArtifactKind
import org.omg.oti.json.common.OTIPrimitiveTypes._

import org.omg.oti.uml._
import org.omg.oti.uml.characteristics._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations.UMLOps
import org.omg.oti.uml.xmi._

import scala.language.higherKinds
import scala.reflect.runtime.universe._
import scala.{Boolean,Either,Int,Option,Left,None,Right,Product,Some,StringContext,Tuple2,Unit}
import scala.Predef.{Map =>_, Set =>_,_}
import scala.collection.immutable._
import scala.collection.Iterable

import scalax.collection.config.CoreConfig
import scalax.collection.mutable.ArraySet.Hints
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.constrained._
import scalax.collection.constrained.constraints.NoneConstraint
import scalax.collection.constrained.generic.GraphConstrainedCompanion

import scalaz._, Scalaz._

case class UnresolvedElementCrossReference[Uml <: UML]
(document: Document[Uml],
 relationTriple: RelationTriple[Uml])

class DocumentSetException
(override val message: String,
 override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException(message, cause)

class DocumentEdge[N]
(nodes: Product, val relations: Vector[RelationTriple[_ <: UML]])
  extends DiEdge[N](nodes)
  with ExtendedKey[N]
  with EdgeCopy[DocumentEdge]
  with OuterEdge[N, DocumentEdge] {

  def keyAttributes = relations
  override def copy[NN](newNodes: Product) = DocumentEdge.newEdge[NN]( newNodes, relations )
  override protected def attributesToString = "(" + relations.size + " triples)"
}

object DocumentEdge {

  protected def newEdge[N](nodes: Product, relations: Vector[RelationTriple[_ <: UML]]) =
    new DocumentEdge[N](nodes, relations)

  def apply[N](e: DiEdge[Product with Serializable with N], relations: Vector[RelationTriple[_ <: UML]]) =
    new DocumentEdge[N](NodeProduct(e.from, e.to), relations)

  def unapply(e: DocumentEdge[Document[_ <: UML]]) =
    Some(e)

  def apply[N](from: N, to: N, relations: Vector[RelationTriple[_ <: UML]]): DocumentEdge[N] =
    new DocumentEdge[N](NodeProduct(from, to), relations)

}

/**
  * @todo add support for the possibility that a stereotype tag value may
  *       refer to an element serialized in a different document.
  */
trait DocumentSet[Uml <: UML] {

  val serializableImmutableDocuments: Set[SerializableImmutableDocument[Uml]]
  val serializableMutableDocuments: Set[SerializableMutableDocument[Uml]]
  val loadingMutableDocuments: Set[LoadingMutableDocument[Uml]]
  val builtInImmutableDocuments: Set[BuiltInImmutableDocument[Uml]]
  val builtInMutableDocuments: Set[BuiltInMutableDocument[Uml]]
  
  val allImmutableDocuments
  : Set[ImmutableDocument[Uml]]
  = serializableImmutableDocuments ++ builtInImmutableDocuments
    
  val allMutableDocuments
  : Set[MutableDocument[Uml]]
  = loadingMutableDocuments ++ serializableMutableDocuments ++ builtInMutableDocuments
    
  val allSerializableDocuments
  : Set[Document[Uml] with SerializableDocument]
  = serializableImmutableDocuments ++ serializableMutableDocuments
    
  val allBuiltInDocuments
  : Set[Document[Uml] with BuiltInDocument]
  = builtInImmutableDocuments ++ builtInMutableDocuments
    
  val allDocuments
  : Set[Document[Uml]]
  = loadingMutableDocuments ++ allSerializableDocuments ++ allBuiltInDocuments
  
  val documentURIMapper: CatalogURIMapper
  val builtInURIMapper: CatalogURIMapper

  val aggregate: Uml#DocumentSetAggregate
  
  implicit val ops: UMLOps[Uml]
  implicit val otiCharacteristicsProvider: OTICharacteristicsProvider[Uml]
  implicit val documentOps: DocumentOps[Uml]
  implicit val nodeT: TypeTag[Document[Uml]]
  implicit val edgeT: TypeTag[DocumentEdge[Document[Uml]]]

  def asBuiltInMutableDocument
  (d: LoadingMutableDocument[Uml],
   artifactKind: OTIArtifactKind)
  : Set[java.lang.Throwable] \/ (BuiltInMutableDocument[Uml], DocumentSet[Uml])
  
  def freezeBuiltInMutableDocument
  (d: BuiltInMutableDocument[Uml])
  : Set[java.lang.Throwable] \/ (BuiltInImmutableDocument[Uml], DocumentSet[Uml])
  
  def asSerializableMutableDocument
  (d: LoadingMutableDocument[Uml],
   artifactKind: OTIArtifactKind)
  : Set[java.lang.Throwable] \/ (SerializableMutableDocument[Uml], DocumentSet[Uml])
  
  def freezeSerializableMutableDocument
  (d: SerializableMutableDocument[Uml])
  : Set[java.lang.Throwable] \/ (SerializableImmutableDocument[Uml], DocumentSet[Uml])

  private val element2document = scala.collection.mutable.HashMap[UMLElement[Uml], Option[Document[Uml]]]()

  def lookupDocumentByExtent
  (e: UMLElement[Uml]): Option[Document[Uml]]
  = this.synchronized {
    element2document
      .getOrElseUpdate(
        e,
        allDocuments.find { d => d.scope == e || d.includes(e) })
  }

  def lookupDocumentByScope
  (e: UMLElement[Uml])
  : Option[Document[Uml]]
  = allDocuments.find { d => d.scope == e }

  def computeElement2DocumentMap
  : Map[UMLElement[Uml], Document[Uml]]
  = this.synchronized {
    element2document
      .flatMap {
        case (e, Some(d)) => Some(e -> d)
        case _ => None
      }
      .toMap
  }

  /**
    * All owned elements of a package excluding the extent of any nested Document scope package
    *
    * @param p a UML Package
    * @return a set, s, such that if e in s, then e is nested in p and e is not in the scope of any Document
    */
  def allOwnedElementsExcludingAllDocumentScopes
  (p: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Set[UMLElement[Uml]]
  = lookupDocumentByExtent(p)
    .fold[Set[java.lang.Throwable] \/ Set[UMLElement[Uml]]]{
      
    val nestedExtent = p.allOwnedElements - p
    val nestedDocumentScopes: Set[UMLPackage[Uml]] = nestedExtent.flatMap {
      case nestedP: UMLPackage[Uml] if lookupDocumentByScope(nestedP).isDefined =>
        Some(nestedP)
      case _ =>
        None
    }
    val restrictedExtent = ( nestedExtent /: nestedDocumentScopes ) { (acc, sub) =>
      acc - sub -- sub.allOwnedElements
    }
    \/-(Set[UMLElement[Uml]](p) ++ restrictedExtent)
  }{ d =>
    -\/(
      Set(
          UMLError.umlAdaptationError(
              s"allOwnedElementsExcludingAllDocumentScopes: p=${p.qualifiedName.get} "+
              s"is already within the scope of a document: ${d.info}")))
  }
  
  implicit val myConfig = CoreConfig(orderHint = 5000, Hints(64, 0, 64, 75))

  class TConnected[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]]
  (val factory: GraphConstrainedCompanion[CC]) {
    implicit val config: Config = NoneConstraint

    def empty() = factory[Document[Uml], DocumentEdge]()
  }

  type MutableDocumentSetGraph = mutable.Graph[Document[Uml], DocumentEdge]
  val mGraphFactory = new TConnected[mutable.Graph](mutable.Graph)

  type ImmutableDocumentSetGraph = immutable.Graph[Document[Uml], DocumentEdge]
  val iGraphFactory = new TConnected[immutable.Graph](immutable.Graph)

  /**
    * Construct the graph of document (nodes) and cross-references among documents (edges) and
    * determine unresolvable cross-references
    *
    * @param ignoreCrossReferencedElementFilter A predicate determining which elements or 
    *                                           element references to ignore
    * @param unresolvedElementMapper A partial function mapping unresolved elements to resolvable elements
    * @return A fail-lazy pair (`scalaz.\&/`):
    *         - optionally, errors encountered during the resolution process
    *         - optionally, a pair of:
    *         1) a [[ResolvedDocumentSet]] constructed from the [[DocumentSet]]'s document nodes & edges
    *         2) [[UnresolvedElementCrossReference]] information about unresolved element cross-references
    */
  def resolve
  (ignoreCrossReferencedElementFilter
   : UMLElement[Uml] => Boolean,

   unresolvedElementMapper
   : UMLElement[Uml] => Option[UMLElement[Uml]],

   includeAllForwardRelationTriple
   : (Document[Uml], RelationTriple[Uml], Document[Uml]) => Boolean = DocumentSet.includeAllForwardRelationTriple[Uml],

   progressTelemetry
   : DocumentResolverProgressTelemetry)
  : Set[java.lang.Throwable] \&/
    (ResolvedDocumentSet[Uml], Vector[UnresolvedElementCrossReference[Uml]])
  = this.synchronized {

    val g = mGraphFactory.empty()

    var remaining: scala.Int = allDocuments.size
    progressTelemetry.scanStarted(DocumentResolverProgressTelemetry.NumberOfDocuments(remaining))

    val t0: scala.Long = java.lang.System.currentTimeMillis()
    var time: scala.Long = t0

    val documentSize = scala.collection.mutable.HashMap[Document[Uml], Int]()
    var count: Int = 0
    var tcount: Int = 0
    // add each document as a node in the graph
    allDocuments foreach { d =>

      progressTelemetry.scanDocumentStarted(
        DocumentResolverProgressTelemetry.DocumentURL(d.documentURL.toString))

      time = java.lang.System.currentTimeMillis()

      g += d
      val prev = count
      d.extent.foreach { e =>
        element2document += e -> Some(d)
        count += 1
      }
      val size = count - prev
      documentSize += d -> size

      val now = java.lang.System.currentTimeMillis()
      val duration = prettyFiniteDuration(now - time, java.util.concurrent.TimeUnit.MILLISECONDS)
      remaining = remaining - 1

      progressTelemetry.scanDocumentEnded(
        DocumentResolverProgressTelemetry.NumberOfElements(size),
        DocumentResolverProgressTelemetry.Duration(duration))

    }

    progressTelemetry.scanEnded(
      DocumentResolverProgressTelemetry.NumberOfDocuments(allDocuments.size),
      DocumentResolverProgressTelemetry.NumberOfElements(count),
      DocumentResolverProgressTelemetry.Duration(
        prettyFiniteDuration(
          java.lang.System.currentTimeMillis() - t0,
          java.util.concurrent.TimeUnit.MILLISECONDS)))

    val unresolvedElementCrossReferences =
      scala.collection.mutable.Queue[UnresolvedElementCrossReference[Uml]]()

    val documentPairs2RelationTriples
    : Map[Document[Uml], Map[Document[Uml], scala.collection.mutable.Queue[RelationTriple[Uml]]]]
    = allDocuments
      .map { di =>

        val diMap
        : Map[Document[Uml], scala.collection.mutable.Queue[RelationTriple[Uml]]]
        = allDocuments
          .flatMap { dj =>
            if (di != dj)
              Some(dj -> scala.collection.mutable.Queue[RelationTriple[Uml]]())
            else
              None
          }
          .toMap

        di -> diMap
      }
      .toMap

    def getDocumentHyperedge
    (di: Document[Uml], dj: Document[Uml])
    : scala.collection.mutable.Queue[RelationTriple[Uml]]
    = documentPairs2RelationTriples(di)(dj)

    val errors = scala.collection.mutable.HashSet[java.lang.Throwable]()

    def addErrors
    (nels: Set[java.lang.Throwable])
    : Unit
    = {
      errors ++= nels
      ()
    }

    def addTriples
    (d: Document[Uml])
    (triples: Set[RelationTriple[Uml]])
    : Unit
    = triples.foreach { t =>
      lookupDocumentByExtent(t.obj)
        .fold[Unit] {
        unresolvedElementCrossReferences += UnresolvedElementCrossReference(d, t)
        ()
      } { dRef =>
        if (d != dRef && includeAllForwardRelationTriple(d, t, dRef)) {
          val dPairTriples = getDocumentHyperedge(d, dRef)
          dPairTriples += t
          tcount = tcount + 1
        }
        ()
      }
    }

    def addElementForwardRelationTriples
    (d: Document[Uml],
     e: UMLElement[Uml])
    : Unit
    = e.forwardRelationTriples().fold(addErrors,addTriples(d))

    remaining = allDocuments.size
    progressTelemetry.resolveStarted(
      DocumentResolverProgressTelemetry.NumberOfDocuments(remaining))

    val r0 = java.lang.System.currentTimeMillis()

    allDocuments.foreach { d =>

      remaining = remaining - 1

      System.out.println(s"# ($remaining documents left) => Resolving ${d.documentURL} ")

      val size = documentSize(d)
      val chunk = {
        val c10 = size / 10
        val c100 = size / 100
        if (c100 > 100)
          c100
        else if (c10 > 10)
          c10
        else
          1
      }

      progressTelemetry.resolveDocumentStarted(
        DocumentResolverProgressTelemetry.DocumentURL(d.documentURL.toString),
        DocumentResolverProgressTelemetry.NumberOfElements(size))

      var count: Int = 0

      var base = java.lang.System.currentTimeMillis()

      d.extent.foreach { e =>

        addElementForwardRelationTriples(d, e)

        count = count + 1
        if (0 == count % chunk) {
          val step = java.lang.System.currentTimeMillis()
          val duration = prettyFiniteDuration(step - base, java.util.concurrent.TimeUnit.MILLISECONDS)

          progressTelemetry.resolveDocumentStepped(
            DocumentResolverProgressTelemetry.NumberOfElements(count),
            DocumentResolverProgressTelemetry.NumberOfElements(size),
            DocumentResolverProgressTelemetry.NumberOfTriples(tcount),
            DocumentResolverProgressTelemetry.Duration(duration))

          base = step
        }

      }

      {
        val now = java.lang.System.currentTimeMillis()
        val duration = prettyFiniteDuration(now - time, java.util.concurrent.TimeUnit.MILLISECONDS)

        progressTelemetry.resolveDocumentEnded(
          DocumentResolverProgressTelemetry.NumberOfElements(count),
          DocumentResolverProgressTelemetry.NumberOfTriples(tcount),
          DocumentResolverProgressTelemetry.Duration(duration))

        time = now
      }

    }

    var hyperEdgeCount: Int = 0

    time = java.lang.System.currentTimeMillis()

    documentPairs2RelationTriples.foreach { case (dFrom, diMap) =>
      diMap.foreach { case (dTo, triples) =>
        if (triples.nonEmpty) {
          hyperEdgeCount = hyperEdgeCount + 1
          // For each inter-document edge: dFrom ~> dTo, record all the RelationTripes(subject, object)
          // where subject is in dFrom and object is in dTo as the justification for the edge.
          g += DocumentEdge(dFrom, dTo, triples.to[Vector])
        }
      }
    }

    {
      val now = java.lang.System.currentTimeMillis()
      val duration = prettyFiniteDuration(now - time, java.util.concurrent.TimeUnit.MILLISECONDS)

      progressTelemetry.resolveEnded(
        DocumentResolverProgressTelemetry.NumberOfDocuments(allDocuments.size),
        DocumentResolverProgressTelemetry.NumberOfElements(count),
        DocumentResolverProgressTelemetry.NumberOfTriples(tcount),
        DocumentResolverProgressTelemetry.NumberOfHyperEdges(hyperEdgeCount),
        DocumentResolverProgressTelemetry.NumberOfUnresolvedCrossReferences(unresolvedElementCrossReferences.size),
        DocumentResolverProgressTelemetry.Duration(duration))

      time = now
    }

    val result = Tuple2(ResolvedDocumentSet(this, g, unresolvedElementMapper), unresolvedElementCrossReferences.to[Vector])

    if (errors.isEmpty)
      \&/.That(result)
    else
      \&/.Both(errors.to[Set], result)
  }

  /**
    * Topologically sort the graph of OTI documents w.r.t their cross-references.
    *
    * @param g Graph.
    * @return Either:
    *         - Left(document) if there is a cycle involving the document's scoped package
    *         - Right(documents) if the documents can be topologically sorted w.r.t. their cross-references
    *
    * It is unclear how to use the topologicalSort in the graph library
    * @see https://groups.google.com/forum/#!searchin/scala-graph/typetag/scala-graph/2x207RGtBSE/ipbLAUwcM0EJ
    *
    * This is simpler:
    * @see https://groups.google.com/d/msg/scala-graph/o-XLlCEC66o/pIpXzOTmwAMJ
    */
  def topologicalSort
  (g: ImmutableDocumentSetGraph)
  : Either[Document[Uml], List[Document[Uml]]] =
    searchAll(g.nodes, IMemo())
      .right
      .map(_.sorted.map(_.value))

  case class IMemo
  ( sorted: List[Document[Uml]] = Nil,
    grey: ImmutableDocumentSetGraph = iGraphFactory.empty(),
    black: ImmutableDocumentSetGraph = iGraphFactory.empty() )

  def dfs
  (node: ImmutableDocumentSetGraph#NodeT, memo: IMemo)
  : Either[Document[Uml], IMemo] = {
    if (memo.grey.contains(OuterNode(node.value)))
      Left[Document[Uml], IMemo]( node.value ) // Cycle involving node.value
    else if (memo.black.contains(OuterNode(node.value)))
      right(memo)
    else
      searchAll(
        node.outNeighbors.toIterable,
        memo.copy(grey = memo.grey + node))
        .right
        .map { a =>
          IMemo(node.value :: a.sorted, memo.grey, a.black + node)
        }
  }

  def searchAll
  (nodes: Iterable[ImmutableDocumentSetGraph#NodeT], memo: IMemo)
  : Either[Document[Uml], IMemo] = {
    ( right( memo ) /: nodes ) {
      ( accu, node ) =>
        accu.right.flatMap( m => dfs( node, m ) )
    }
  }

  def right(m: IMemo): Either[Document[Uml], IMemo] =
    Right(m)

  /**
    * Topologically sort the graph of OTI documents w.r.t their cross-references.
    *
    * @param g Graph.
    * @return Either:
    *         - Left(document) if there is a cycle involving the document's scoped package
    *         - Right(documents) if the documents can be topologically sorted w.r.t. their cross-references
    *
    * It is unclear how to use the topologicalSort in the graph library
    * @see https://groups.google.com/forum/#!searchin/scala-graph/typetag/scala-graph/2x207RGtBSE/ipbLAUwcM0EJ
    *
    * This is simpler:
    * @see https://groups.google.com/d/msg/scala-graph/o-XLlCEC66o/pIpXzOTmwAMJ
    */
  def topologicalSort
  (g: MutableDocumentSetGraph)
  : Either[Document[Uml], List[Document[Uml]]] =
    searchAll(g.nodes, MMemo())
    .right
    .map(_.sorted.map(_.value))

  case class MMemo
  ( sorted: List[Document[Uml]] = Nil,
    grey: MutableDocumentSetGraph = mGraphFactory.empty(),
    black: MutableDocumentSetGraph = mGraphFactory.empty() )

  def dfs
  (node: MutableDocumentSetGraph#NodeT, memo: MMemo)
  : Either[Document[Uml], MMemo] = {
    if (memo.grey.contains(OuterNode(node.value)))
      Left[Document[Uml], MMemo]( node.value ) // Cycle involving node.value
    else if (memo.black.contains(OuterNode(node.value)))
      right(memo)
    else
      searchAll(
        node.outNeighbors.toIterable,
        memo.copy(grey = memo.grey + node))
      .right
      .map { a =>
        MMemo(node.value :: a.sorted, memo.grey, a.black + node)
      }
  }

  def searchAll
  (nodes: Iterable[MutableDocumentSetGraph#NodeT], memo: MMemo)
  : Either[Document[Uml], MMemo] = {
    ( right( memo ) /: nodes ) {
      ( accu, node ) =>
      accu.right.flatMap( m => dfs( node, m ) )
    }
  }

  def right(m: MMemo): Either[Document[Uml], MMemo] =
    Right(m)

}

object DocumentSet {

  def includeAllForwardRelationTriple[Uml <: UML]
  (dFrom: Document[Uml], triple: RelationTriple[Uml], dTo: Document[Uml])
  : Boolean = true
  
  val XMI_Version = "20131001"

  val XMI_ns = "http://www.omg.org/spec/XMI/20131001"
  val XSI_ns = "http://www.w3.org/2001/XMLSchema-instance"
  val UML_ns = "http://www.omg.org/spec/UML/20131001"
  val MOFEXT_ns = "http://www.omg.org/spec/MOF/20131001"

  /**
   * Check http://solitaire.omg.org/browse/TIWG-3
   */
  def serializeValueSpecificationAsTagValue[Uml <: UML]
  (value: UMLValueSpecification[Uml])
  (implicit idg: IDGenerator[Uml])
  : Set[java.lang.Throwable] \/ Option[String] =
    value match {
      case l: UMLLiteralBoolean[Uml] =>
        l.value.toString.some.right
      case l: UMLLiteralInteger[Uml] =>
        l.value.toString.some.right
      case l: UMLLiteralReal[Uml] =>
        l.value.toString.some.right
      case l: UMLLiteralString[Uml] =>
        l.value.right
      case iv: UMLInstanceValue[Uml] =>
        iv.instance
        .fold[Set[java.lang.Throwable] \/ Option[String]](
          Option.empty[String].right
        ){ is =>
          is
            .xmiID()(idg)
            .map(OTI_ID.unwrap(_).some)
        }
      case v =>
        Set(
          UMLError
          .illegalElementError[Uml, UMLValueSpecification[Uml]](
            s"No value=>string serialization support for ${v.xmiType.head} (ID=${v.xmiID})",
            scala.collection.immutable.Iterable(value)))
        .left
    }

  def isPackageRootOfSpecificationDocument[Uml <: UML]
  (pkg: UMLPackage[Uml])
  (implicit otiCharacteristicsProvider: OTICharacteristicsProvider[Uml])
  : Set[java.lang.Throwable] \/ Boolean =
  for {
    pkgURI <- pkg.getEffectiveURI
    docURL <- pkg.getDocumentURL()
  } yield
    pkgURI.isDefined &&
    docURL.isDefined

}