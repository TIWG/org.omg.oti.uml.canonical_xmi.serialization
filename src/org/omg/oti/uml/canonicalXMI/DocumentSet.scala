/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.uml.canonicalXMI

import java.io.{BufferedWriter,FileWriter,PrintWriter,Serializable}
import java.lang.{IllegalArgumentException,System}

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.characteristics._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations.UMLOps
import org.omg.oti.uml.xmi._

import scala.annotation.tailrec
import scala.language.{higherKinds,implicitConversions,postfixOps}
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.{Boolean,Either,Function1,Option,Left,None,Right,Product,Some,StringContext}
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
import scalax.collection.io.edge.CEdgeParameters
import scalax.collection.io.json.descriptor.CEdgeDescriptor
import scalax.collection.io.json.descriptor.NodeDescriptor

import scalaz._, Scalaz._

/**
 * There seems to be a bug in scala-graph core 1.9.1
 * @see https://github.com/scala-graph/scala-graph/issues/29
 *
 *      As a workaround, the edge is defined as a kind of directed hyperedge (DiHyperEdge) but
 *      this is overkill, it should be just a directed edge (DiEdge)
 */
class DocumentEdge[N](nodes: Product)
  extends DiHyperEdge[N](nodes) // DiEdge
          with EdgeCopy[DocumentEdge]
          with OuterEdge[N, DocumentEdge] {

  override def copy[NN](newNodes: Product) = DocumentEdge.newEdge[NN]( newNodes )
}

object DocumentEdge extends EdgeCompanion[DocumentEdge] {
  protected def newEdge[N](nodes: Product) = new DocumentEdge[N](nodes)

  /**
   * @see https://github.com/scala-graph/scala-graph/issues/29
   *      should be DiEdge[Project with ... ]
   */
  def apply[Uml <: UML](e: DiHyperEdge[Product with Serializable with Document[Uml]]) =
    new DocumentEdge[Document[Uml]](NodeProduct(e.from, e.to))

  def apply[Uml <: UML](from: Document[Uml], to: Document[Uml]) =
    new DocumentEdge[Document[Uml]](NodeProduct(from, to))

  def unapply[Uml <: UML](e: DocumentEdge[Document[Uml]]) =
    Some(e)

  def apply[N](from: N, to: N): DocumentEdge[N] =
    new DocumentEdge[N](NodeProduct(from, to))

  override def from[N](nodes: Product): DocumentEdge[N] =
    new DocumentEdge[N](NodeProduct(nodes.productElement(1), nodes.productElement(2)))
}

case class UnresolvedElementCrossReference[Uml <: UML]
(document: Document[Uml],
 documentElement: UMLElement[Uml],
 externalReference: UMLElement[Uml])

class DocumentSetException
(override val message: String,
 override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException(message, cause)

/**
 * @todo add support for the possibility that a stereotype tag value may
 *       refer to an element serialized in a different document.
 */
trait DocumentSet[Uml <: UML] {
  val serializableDocuments: Set[SerializableDocument[Uml]]
  val builtInDocuments: Set[BuiltInDocument[Uml]]
  val builtInDocumentEdges: Set[DocumentEdge[Document[Uml]]]
  val documentURIMapper: CatalogURIMapper
  val builtInURIMapper: CatalogURIMapper

  val aggregate: Uml#DocumentSetAggregate
  
  implicit val ops: UMLOps[Uml]
  implicit val otiCharacteristicsProvider: OTICharacteristicsProvider[Uml]
  implicit val documentOps: DocumentOps[Uml]
  implicit val nodeT: TypeTag[Document[Uml]]
  implicit val edgeT: TypeTag[DocumentEdge[Document[Uml]]]

  implicit val myConfig = CoreConfig(orderHint = 5000, Hints(64, 0, 64, 75))

  //implicit val documentEdgeTag: TypeTag[DocumentEdge[Document[Uml]]] = typeTag[DocumentEdge[Document[Uml]]]

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
   * @param ignoreCrossReferencedElementFilter A predicate determing whether to ignore
   *                                           an element or a cross reference to an element.
   *
   * @return a 3-tuple:
   *         - the graph of document-level cross references
   *         - the map of elements to theirs serialization document
   *         - the unresolved cross references
   */
  def resolve
  (ignoreCrossReferencedElementFilter: UMLElement[Uml] => Boolean,
   unresolvedElementMapper: UMLElement[Uml] => Option[UMLElement[Uml]])
  : NonEmptyList[java.lang.Throwable] \/ (ResolvedDocumentSet[Uml], Iterable[UnresolvedElementCrossReference[Uml]]) = {

    val allDocuments = serializableDocuments ++ builtInDocuments

    val element2document: Map[UMLElement[Uml], Document[Uml]] = {
      val allE2D: Map[UMLElement[Uml], Document[Uml]] =
        allDocuments
          .flatMap { d =>
            val dExtent: Set[UMLElement[Uml]] =
              d.extent
            val dExtentF: Set[UMLElement[Uml]] =
              dExtent
                .filter { (e) =>
                  !ignoreCrossReferencedElementFilter(e)
                }
            val dMap: Map[UMLElement[Uml], Document[Uml]] =
              dExtentF
                .map { (e) =>
                  (e -> d)
                }
            .toMap
            dMap
          }
      .toMap

      allE2D
    }


    def lookupDocumentForElement
    (e: UMLElement[Uml])
    : Option[Document[Uml]]
    = element2document
      .get(e)
      .orElse {
        if (ignoreCrossReferencedElementFilter(e))
          None
        else
          unresolvedElementMapper(e)
          .flatMap { eMapped =>
            element2document.get(eMapped)
          }
      }

    def lookupDocumentForElementReference
    (d: Document[Uml], e: UMLElement[Uml], eRef: UMLElement[Uml])
    : Either[Document[Uml], UnresolvedElementCrossReference[Uml]]
    = lookupDocumentForElement(eRef)
      .fold[Either[Document[Uml], UnresolvedElementCrossReference[Uml]]] {
        Right(UnresolvedElementCrossReference(d, e, eRef))
      }{ dRef =>
        Left(dRef)
      }

    val g = mGraphFactory.empty()

    // add each document as a node in the graph
    element2document.values foreach { d => g += d }

    // add the edges among built-in documents.
    g ++= builtInDocumentEdges

    val u0: NonEmptyList[java.lang.Throwable] \/ Set[UnresolvedElementCrossReference[Uml]] = Set().right
    val uN: NonEmptyList[java.lang.Throwable] \/ Set[UnresolvedElementCrossReference[Uml]] =
      (u0 /: element2document) { case (ui, (e, d)) =>

      ui +++
      e.allForwardReferencesToImportablePackageableElements
      .map { eRefs =>
        val s = for {
          eRef <- eRefs
          u <- lookupDocumentForElementReference(d, e, eRef) match {
            case Right(unresolved) =>
              Some(unresolved)
            case Left(dRef) =>
              d match {
                case sd: SerializableDocument[Uml] =>
                  g += DocumentEdge(sd, dRef)
                case _ =>
                  ()
              }
              None
          }
        } yield u
        s
      }
    }

    uN.map { unresolved =>
      (ResolvedDocumentSet(
        this,
        g,
        element2document,
        unresolvedElementMapper),
        unresolved)
    }
  }

  /**
   * Adapted from a scala-graph addition that is not yet in 1.9.1
   *
   * @see https://groups.google.com/forum/#!searchin/scala-graph/typetag/scala-graph/2x207RGtBSE/ipbLAUwcM0EJ
   */
  def topologicalSort
  (g: MutableDocumentSetGraph)
  : Either[Document[Uml], List[Document[Uml]]] =
    searchAll(g.nodes, Memo())
    .right
    .map(_.sorted.map(_.value))

  case class Memo
  ( sorted: List[Document[Uml]] = Nil,
    grey: MutableDocumentSetGraph = mGraphFactory.empty(),
    black: MutableDocumentSetGraph = mGraphFactory.empty() )

  def dfs
  (node: MutableDocumentSetGraph#NodeT, memo: Memo)
  : Either[Document[Uml], Memo] = {
    if (memo.grey.contains(OuterNode(node.value)))
      Left[Document[Uml], Memo]( node.value )
    else if (memo.black.contains(OuterNode(node.value)))
      right(memo)
    else
      searchAll(
        node.outNeighbors.toIterable,
        memo.copy(grey = memo.grey + node))
      .right
      .map { a =>
        Memo(node.value :: a.sorted, memo.grey, a.black + node)
      }
  }

  def searchAll
  (nodes: Iterable[MutableDocumentSetGraph#NodeT], memo: Memo)
  : Either[Document[Uml], Memo] = {
    ( right( memo ) /: nodes ) {
      ( accu, node ) =>
      accu.right.flatMap( m => dfs( node, m ) )
    }
  }

  def right(m: Memo): Either[Document[Uml], Memo] =
    Right(m)

}

object DocumentSet {

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
  : NonEmptyList[java.lang.Throwable] \/ Option[String] =
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
        .fold[NonEmptyList[java.lang.Throwable] \/ Option[String]](
          Option.empty[String].right
        ){ is =>
          is
            .xmiID()(idg)
            .map(_.some)
        }
      case v =>
        NonEmptyList(
          UMLError
          .illegalElementError[Uml, UMLValueSpecification[Uml]](
            s"No value=>string serialization support for ${v.xmiType.head} (ID=${v.xmiID})",
            Iterable(value)))
        .left
    }

  def isPackageRootOfSpecificationDocument[Uml <: UML]
  (pkg: UMLPackage[Uml])
  (implicit otiCharacteristicsProvider: OTICharacteristicsProvider[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Boolean =
  for {
    pkgURI <- pkg.getEffectiveURI
    docURL <- pkg.getDocumentURL()
  } yield
    pkgURI.isDefined &&
    docURL.isDefined

  def constructDocumentSetCrossReferenceGraph[Uml <: UML]
  (specificationRootPackages: Set[UMLPackage[Uml]],
   documentURIMapper: CatalogURIMapper,
   builtInURIMapper: CatalogURIMapper,
   builtInDocuments: Set[BuiltInDocument[Uml]],
   builtInDocumentEdges: Set[DocumentEdge[Document[Uml]]],
   ignoreCrossReferencedElementFilter: Function1[UMLElement[Uml], Boolean],
   unresolvedElementMapper: UMLElement[Uml] => Option[UMLElement[Uml]],
   aggregate: Uml#DocumentSetAggregate)
  (implicit
   ops: UMLOps[Uml],
   documentOps: DocumentOps[Uml],
   nodeT: TypeTag[Document[Uml]],
   edgeT: TypeTag[DocumentEdge[Document[Uml]]])
  : NonEmptyList[java.lang.Throwable] \&/ (ResolvedDocumentSet[Uml], Iterable[UnresolvedElementCrossReference[Uml]]) = {

    import documentOps._

    object ResultSetAggregator {
      def zero[A]:  NonEmptyList[java.lang.Throwable] \&/ Set[A] = \&/.That(Set[A]())
    }

    trait ResultSetAggregator[A] extends Monoid[NonEmptyList[java.lang.Throwable] \&/ Set[A]] {
      type F = NonEmptyList[java.lang.Throwable] \&/ Set[A]
      override def zero: F = ResultSetAggregator.zero[A]
      override def append(f1: F, f2: => F): F = f1 append f2
    }

    val roots: ResultSetAggregator[UMLPackage[Uml]]#F =
      ( ResultSetAggregator.zero[UMLPackage[Uml]] /: specificationRootPackages ) { (acc, pkg) =>
        acc append
        isPackageRootOfSpecificationDocument(pkg)
        .map { ok =>
          if (ok)
            Set[UMLPackage[Uml]](pkg)
          else
            Set[UMLPackage[Uml]]()
        }
        .toThese
      }

    val documents: ResultSetAggregator[SerializableDocument[Uml]]#F =
      roots.flatMap { _roots =>
        ( ResultSetAggregator.zero[SerializableDocument[Uml]] /: _roots ) { (acc, root) =>
          acc append
          createSerializableDocumentFromExistingRootPackage(root).map(Set(_))
          .toThese
        }
      }

    val result
    :  NonEmptyList[java.lang.Throwable] \&/ (ResolvedDocumentSet[Uml], Iterable[UnresolvedElementCrossReference[Uml]]) =
    documents
    .flatMap { serializableDocuments =>

      val ds
      : NonEmptyList[java.lang.Throwable] \/ (ResolvedDocumentSet[Uml], Iterable[UnresolvedElementCrossReference[Uml]]) =
      createDocumentSet(
        serializableDocuments,
        builtInDocuments,
        builtInDocumentEdges,
        documentURIMapper,
        builtInURIMapper,
        aggregate)
        .flatMap { ds =>
          ds
          .resolve(
              ignoreCrossReferencedElementFilter,
              unresolvedElementMapper)

        }

      ds.toThese
    }

    result
  }
}
