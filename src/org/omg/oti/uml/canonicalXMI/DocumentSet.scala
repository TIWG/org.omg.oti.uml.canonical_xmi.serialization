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

import org.omg.oti.uml.OTIPrimitiveTypes._
import org.omg.oti.uml._
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

case class UnresolvedElementCrossReference[Uml <: UML]
(document: Document[Uml],
 relationTriple: RelationTriple[Uml])

class DocumentSetException
(override val message: String,
 override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException(message, cause)

class DocumentEdge[N](nodes: Product, val relations: Seq[RelationTriple[_ <: UML]])
  extends DiEdge[N](nodes)
  with ExtendedKey[N]
  with EdgeCopy[DocumentEdge]
  with OuterEdge[N, DocumentEdge] {

  def keyAttributes = Seq(relations)
  override def copy[NN](newNodes: Product) = DocumentEdge.newEdge[NN]( newNodes, relations )
  override protected def attributesToString = "(" + relations.size + " triples)"
}

object DocumentEdge {

  protected def newEdge[N](nodes: Product, relations: Seq[RelationTriple[_ <: UML]]) =
    new DocumentEdge[N](nodes, relations)

  def apply[N](e: DiEdge[Product with Serializable with N], relations: Seq[RelationTriple[_ <: UML]]) =
    new DocumentEdge[N](NodeProduct(e.from, e.to), relations)

  def unapply(e: DocumentEdge[Document[_ <: UML]]) =
    Some(e)

  def apply[N](from: N, to: N, relations: Seq[RelationTriple[_ <: UML]]): DocumentEdge[N] =
    new DocumentEdge[N](NodeProduct(from, to), relations)

}

/**
  * @todo add support for the possibility that a stereotype tag value may
  *       refer to an element serialized in a different document.
  */
trait DocumentSet[Uml <: UML] {

  val serializableImmutableDocuments: Set[SerializableImmutableDocument[Uml]]
  val serializableMutableDocuments: Set[SerializableMutableDocument[Uml]]
  val builtInImmutableDocuments: Set[BuiltInImmutableDocument[Uml]]
  val builtInMutableDocuments: Set[BuiltInMutableDocument[Uml]]
  
  val allImmutableDocuments: Set[ImmutableDocument[Uml]] =
    serializableImmutableDocuments ++ builtInImmutableDocuments
    
  val allMutableDocuments: Set[MutableDocument[Uml]] =
    serializableMutableDocuments ++ builtInMutableDocuments
    
  val allSerializableDocuments: Set[Document[Uml]] = 
    serializableImmutableDocuments ++ serializableMutableDocuments
    
  val allBuiltInDocuments: Set[Document[Uml]] =
    builtInImmutableDocuments ++ builtInMutableDocuments
    
  val allDocuments: Set[Document[Uml]] = allSerializableDocuments ++ allBuiltInDocuments
  
  val documentURIMapper: CatalogURIMapper
  val builtInURIMapper: CatalogURIMapper

  val aggregate: Uml#DocumentSetAggregate
  
  implicit val ops: UMLOps[Uml]
  implicit val otiCharacteristicsProvider: OTICharacteristicsProvider[Uml]
  implicit val documentOps: DocumentOps[Uml]
  implicit val nodeT: TypeTag[Document[Uml]]
  implicit val edgeT: TypeTag[DocumentEdge[Document[Uml]]]

  val element2ImmutableDocument: Map[UMLElement[Uml], ImmutableDocument[Uml]] = {
    val m1 =
      for { 
        d <- serializableImmutableDocuments
        e <- d.extent
      } yield (e -> d)
      
    val m2 =
      for { 
        d <- builtInImmutableDocuments
        e <- d.extent
      } yield (e -> d)
      
    (m1 ++ m2).toMap
  }
  
  def freezeBuiltInMutableDocument
  (d: BuiltInMutableDocument[Uml])
  : NonEmptyList[java.lang.Throwable] \/ (BuiltInImmutableDocument[Uml], DocumentSet[Uml])
  
  def freezeSerializableMutableDocument
  (d: SerializableMutableDocument[Uml])
  : NonEmptyList[java.lang.Throwable] \/ (SerializableImmutableDocument[Uml], DocumentSet[Uml])
  
  def lookupDocumentByExtent(e: UMLElement[Uml]): Option[Document[Uml]] =
    element2ImmutableDocument.get(e)
    .orElse(allMutableDocuments.find(d => d.includes(e)))
    
  def lookupDocumentByScope(e: UMLElement[Uml]): Option[Document[Uml]] =
    allDocuments.find(d => d.scope == e)
      
  def computeElement2DocumentMap: Map[UMLElement[Uml], Document[Uml]] = 
     allDocuments.flatMap { d => d.extent.map(_ -> d).toMap }.toMap
     
  /**
    * All owned elements of a package excluding the extent of any nested Document scope package
    * @param p a UML Package
    * @return a set, s, such that if e in s, then e is nested in p and e is not in the scope of any Document
    */
  def allOwnedElementsExcludingAllDocumentScopes
  (p: UMLPackage[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Set[UMLElement[Uml]] = 
    lookupDocumentByExtent(p)
    .fold[NonEmptyList[java.lang.Throwable] \/ Set[UMLElement[Uml]]]{
      
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
      NonEmptyList(
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
    * @return A fail-lazy pair ([[scalaz.\&/]]):
    *         - optionally, errors encountered during the resolution process
    *         - optionally, a pair of:
    *         1) a [[ResolvedDocumentSet]] constructed from the [[DocumentSet]]'s document nodes & edges
    *         2) [[UnresolvedElementCrossReference]] information about unresolved element cross-references
    */
  def resolve
  (ignoreCrossReferencedElementFilter: UMLElement[Uml] => Boolean,
   unresolvedElementMapper: UMLElement[Uml] => Option[UMLElement[Uml]],
   includeAllForwardRelationTriple: (Document[Uml], RelationTriple[Uml], Document[Uml]) => Boolean =
     DocumentSet.includeAllForwardRelationTriple[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ 
    (ResolvedDocumentSet[Uml], Iterable[UnresolvedElementCrossReference[Uml]]) = {

    val e2d: Map[UMLElement[Uml], Document[Uml]] = computeElement2DocumentMap

    def lookupDocumentForElement
    (e: UMLElement[Uml])
    : Option[Document[Uml]]
    = e2d
      .get(e)
      .orElse {
        if (ignoreCrossReferencedElementFilter(e))
          None
        else
          unresolvedElementMapper(e)
          .flatMap { eMapped =>
            e2d.get(eMapped)
          }
      }

    val g = mGraphFactory.empty()

    // add each document as a node in the graph
    e2d.values foreach { d => g += d }

    type UnresolvedElementCrossReferences = Set[UnresolvedElementCrossReference[Uml]]
    type Document2RelationTriples = Map[Document[Uml], Seq[RelationTriple[Uml]]]
    type DocumentPair2RelationTriplesUnresolvedElementCrossReferences =
    (Map[(Document[Uml], Document[Uml]), Seq[RelationTriple[Uml]]], UnresolvedElementCrossReferences)

    val u0: NonEmptyList[java.lang.Throwable] \&/
            DocumentPair2RelationTriplesUnresolvedElementCrossReferences =
      \&/.That(
        ( Map[(Document[Uml], Document[Uml]), Seq[RelationTriple[Uml]]](),
          Set[UnresolvedElementCrossReference[Uml]]()
        ))
    val empty_uxref = Set[UnresolvedElementCrossReference[Uml]]()
    val empty_d2triples = Map[Document[Uml], Seq[RelationTriple[Uml]]]()

    val uN: NonEmptyList[java.lang.Throwable] \&/ 
            DocumentPair2RelationTriplesUnresolvedElementCrossReferences =
      (u0 /: e2d) { case (ui, (e, d)) =>

        ui.flatMap { case (dPair2relationTriples1, unresolvedXRefs1) =>

          e.forwardRelationTriples()
            .toThese
            .map { triples =>

              val (unresolvedXRefs2: UnresolvedElementCrossReferences, 
                   triplesByDocument: Document2RelationTriples) =
                ((empty_uxref, empty_d2triples) /: triples) {
                  case ((us, d2ts), t) =>
                    lookupDocumentForElement(t.obj)
                      .fold[(UnresolvedElementCrossReferences, Document2RelationTriples)](
                      (us + UnresolvedElementCrossReference(d, t), d2ts)
                    ) { dRef =>
                      if (includeAllForwardRelationTriple(d, t, dRef))
                        (us, d2ts.updated(dRef, t +: d2ts.getOrElse(dRef, Seq[RelationTriple[Uml]]())))
                      else
                        (us, d2ts)    
                    }
                }

              val dPair2relationTriples2 = (dPair2relationTriples1 /: triplesByDocument) {
                case (dPair2triples, (dRef, refTriples)) =>
                  if (d == dRef)
                    // do not create a self-edge for intra-document reference triples
                    dPair2triples
                  else {
                    // only add inter-document edges with the reference triples
                    val key = (d, dRef)
                    dPair2triples.updated(
                      key,
                      value = dPair2triples.getOrElse(key, Seq[RelationTriple[Uml]]()) ++ refTriples
                    )
                  }
              }

              (dPair2relationTriples2, unresolvedXRefs1 ++ unresolvedXRefs2)
            }
        }
      }

    uN.map { case (dPair2relationTriples, unresolved) =>
      dPair2relationTriples.foreach { case ((dFrom, dTo), triples) =>
        // For each inter-document edge: dFrom ~> dTo, record all the RelationTripes(subject, object)
        // where subject is in dFrom and object is in dTo as the justification for the edge.
        g += DocumentEdge(dFrom, dTo, triples)
      }
      (ResolvedDocumentSet(
        this,
        g,
        e2d,
        unresolvedElementMapper),
        unresolved)
    }
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
            .map(OTI_ID.unwrap(_).some)
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

}
