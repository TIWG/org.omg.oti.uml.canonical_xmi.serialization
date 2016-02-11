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

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.characteristics.{OTISpecificationRootCharacteristics, OTICharacteristicsProvider}
import org.omg.oti.uml.xmi._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.collection.immutable._
import scala.reflect.runtime.universe._
import scala.Predef.{identity,String}
import scala.StringContext
import scalaz._

import java.io.InputStream
import java.net.URI

class DocumentOpsException[Uml <: UML]
( dOps: DocumentOps[Uml],
  override val message: String,
  override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException(message, cause) {

  /**
   * This type member is intended to facilitate pattern matching
   * using a wildcard for the type parameter, i.e., DocumentOpsException[_]
   * The type information can then be checked using the UmlType member.
   */
  type UmlType = Uml
}

/**
 * OMG Tool-neutral API extension for document-related processing of OMG UML 2.5 compliant models
 *
 * @see OMG XMI 2.5, formal/2015-06-07 http://www.omg.org/spec/XMI/2.5.1
 *
 * @tparam Uml Type signature of a tool-specific adaptation of OMG UML 2.5
 */
trait DocumentOps[Uml <: UML] {

  implicit val otiCharacteristicsProvider: OTICharacteristicsProvider[Uml]

  /**
    * Get the URI of the document as an externally accessible resource.
    *
    * @param lurl The `LoadURL` coordinates of the external document to load
    * @return The URI where the document can be accesssed as an external resource
    */
  def getExternalDocumentURL
  (lurl: Uml#LoadURL)
  : NonEmptyList[java.lang.Throwable] \/ URI

  /**
    * Open an input stream on the external document to load
    * @param lurl The `LoadURL` coordinates of the external document to load
    * @return an input stream for reading the XMI contents of the external document to load
    */
  def openExternalDocumentStreamForImport
  (lurl: Uml#LoadURL)
  : NonEmptyList[java.lang.Throwable] \/ InputStream

  /**
    * Add a set of Documents to a DocumentSet
    *
    * Note that for performance reasons, the return type is `Set[Throwable] \&/ DocumentSet[Uml]` (fast merge)
    * rather than the `NonEmptyList[Throwable] \&/ DocumentSet[Uml]` (slow merge).
    *
    * @param ds DocumentSet
    * @param d a set of Documents
    * @return The DocumentSet resulting from adding each Document di in d where di can be successfully added.
    */
  def addDocuments
  (ds: DocumentSet[Uml],
   d: Set[Document[Uml]])
  : Set[java.lang.Throwable] \&/ DocumentSet[Uml] = {

    val r0: Set[java.lang.Throwable] \&/ DocumentSet[Uml] = \&/.That(ds)
    val rN: Set[java.lang.Throwable] \&/ DocumentSet[Uml] = (r0 /: d) { case (acc, di) =>
        addDocument(ds, di)
          .fold[Set[java.lang.Throwable] \&/ DocumentSet[Uml]](
          l = (nels: NonEmptyList[java.lang.Throwable]) =>
            acc.bimap[Set[java.lang.Throwable], DocumentSet[Uml]](
              f = (ness: Set[java.lang.Throwable]) => ness ++ nels.list,
              g = identity
            ),
          r = (dsi: DocumentSet[Uml]) =>
            acc.onlyThis.fold[Set[java.lang.Throwable] \&/ DocumentSet[Uml]](\&/.That(dsi)){
              nels =>
                \&/.Both(nels, dsi)
            }
        )
    }
    rN
  }

  /**
    * Add a document as a new node to an existing document set graph
    *
    * @param ds A DocumentSet graph
    * @param d A Document to add as a new node to `ds`
    * @return A new DocumentSet, ds', whose nodes should be ds'=d + ds.documents
    */
  def addDocument
  (ds: DocumentSet[Uml],
   d: Document[Uml])
  : NonEmptyList[java.lang.Throwable] \/ DocumentSet[Uml]

  def freezeMutableDocument
  (ds: DocumentSet[Uml],
   d: MutableDocument[Uml])
  : NonEmptyList[java.lang.Throwable] \/ (Document[Uml], DocumentSet[Uml]) =
    d match {
    case mD: BuiltInMutableDocument[Uml] =>
      if (!ds.builtInMutableDocuments.contains(mD))
        -\/(NonEmptyList(UMLError.umlAdaptationError(
            s"freezeMutableDocument: mutable document, ${mD.info} is not in the document set!")))
      else 
        ds.freezeBuiltInMutableDocument(mD)
    case mD: SerializableMutableDocument[Uml] =>
      if (!ds.serializableMutableDocuments.contains(mD))
        -\/(NonEmptyList(UMLError.umlAdaptationError(
            s"freezeMutableDocument: mutable document, ${mD.info} is not in the document set!")))
      else 
        ds.freezeSerializableMutableDocument(mD)
  }

  /**
    * Create a BuiltInMutableDocument for a root package scope whose contents are subject to change.
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
    *
    * @param ds DocumentSet
    * @param info the OTI specification characteristics of the `scope` UML Package as the root of an OTI document
    * @param documentURL the `LoadURL` information about the external URL from where
    *                    the contents of built-in document contents correspond to those of the root package
    * @param root a tool-specific root package corresponding to the tool-specific implementation
    *             of an OMG-defined document (e.g., the OMG UML2.5 PrimitiveTypes library)
    * @return A tuple with:
    *         - a BuiltInDocument, `d`
    *         - a DocumentSet, `ds'`, which is `ds` + `d`
    */
  def addBuiltInImmutableDocument
  ( ds: DocumentSet[Uml],
    info: OTISpecificationRootCharacteristics,
    documentURL: Uml#LoadURL,
    root: UMLPackage[Uml],
    builtInExtent: Set[UMLElement[Uml]] )
  : NonEmptyList[java.lang.Throwable] \/ ( BuiltInImmutableDocument[Uml], DocumentSet[Uml] )

  /**
    * Create a BuiltInImmutableDocument for a root package scope whose contents are fully known.
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
    *
    * @param ds DocumentSet
    * @param info the OTI specification characteristics of the `scope` UML Package as the root of an OTI document
    * @param documentURL the `LoadURL` information about the external URL from where
    *                    the contents of built-in document contents correspond to those of the root package
    * @param root a tool-specific root package corresponding to the tool-specific implementation
    *             of an OMG-defined document (e.g., the OMG UML2.5 PrimitiveTypes library)
    * @return A tuple with:
    *         - a BuiltInDocument, `d`
    *         - a DocumentSet, `ds'`, which is `ds` + `d`
    */
  def addBuiltInMutableDocument
  ( ds: DocumentSet[Uml],
    info: OTISpecificationRootCharacteristics,
    documentURL: Uml#LoadURL,
    root: UMLPackage[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ ( BuiltInMutableDocument[Uml], DocumentSet[Uml] )

  /**
    * Create a LoadingMutableDocument for a root package scope whose contents are subject to change
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
    *
    * @param ds DocumentSet
    * @param info the OTI specification characteristics of the `scope` UML Package as the root of an OTI document
    * @param documentURL the `LoadURL` information about the external URL from where
    *                    the serializable document contents will be read into the contents of
    *                    the root package
    * @param root the root package scope of the OTI serializable document
    * @return If successful, a SerializableDocument for the `root` package scope
    */
  def addLoadingMutableDocument
  ( ds: DocumentSet[Uml],
    info: OTISpecificationRootCharacteristics,
    documentURL: Uml#LoadURL,
    root: UMLPackage[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ ( LoadingMutableDocument[Uml], DocumentSet[Uml] )
  
  /**
    * Create a SerializableImmutableDocument for an existing root package whose contents are fully known
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Creation
    *
    * @param ds DocumentSet
    * @param info the OTI specification characteristics of the `scope` UML Package as the root of an OTI document
    * @param documentURL the `LoadURL` information about the external URL from where
    *                    the serializable document contents will be read into the contents of
    *                    the root package
    * @param root The root package scope of the serializable document
    * @return A SerializableDocument if the `root` package has the necessary information to specify
    *         how it should be eventually serialized per OMG XMI 2.5.1
    */
  def addSerializableImmutableDocument
  ( ds: DocumentSet[Uml],
    info: OTISpecificationRootCharacteristics,
    documentURL: Uml#LoadURL,
    root: UMLPackage[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ ( SerializableImmutableDocument[Uml], DocumentSet[Uml] )

  /**
    * Create a SerializableMutableDocument for a root package scope whose contents are subject to change
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
    *
    * @param ds DocumentSet
    * @param info the OTI specification characteristics of the `scope` UML Package as the root of an OTI document
    * @param documentURL the `LoadURL` information about the external URL from where
    *                    the serializable document contents will be read into the contents of
    *                    the root package
    * @param root the root package scope of the OTI serializable document
    * @return If successful, a SerializableDocument for the `root` package scope
    */
  def addSerializableMutableDocument
  ( ds: DocumentSet[Uml],
    info: OTISpecificationRootCharacteristics,
    documentURL: Uml#LoadURL,
    root: UMLPackage[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ ( SerializableMutableDocument[Uml], DocumentSet[Uml] )

  /**
    * Create a DocumentSet graph for document nodes (serializable or built-in) and inter-document edges
    *
    * @param documents The set of document nodes
    * @param documentURIMapper OASIS XML Catalog-based mapping of package or element URIs
    *                          to serializable document URLs and element URIs
    * @param builtInURIMapper OASIS XML Catalog-based mapping of package or element URIs
    *                         to OMG-published document URLs and element URIs
    * @param aggregate Document Set Aggregate
    * @param ops OTI UML Read Operations API
    * @param nodeT Scala type information about Document[UML] graph nodes
    * @param edgeT Scala type information about Document[UML] to Document[UML] graph edges
    * @return A DocumentSet graph
    */
  def createDocumentSet
  ( documents: Set[Document[Uml]],
    documentURIMapper: CatalogURIMapper,
    builtInURIMapper: CatalogURIMapper,
    aggregate: Uml#DocumentSetAggregate)
  ( implicit
    ops: UMLOps[Uml],
    nodeT: TypeTag[Document[Uml]],
    edgeT: TypeTag[DocumentEdge[Document[Uml]]] )
  : NonEmptyList[java.lang.Throwable] \&/ DocumentSet[Uml]

  /**
   * Create an initial DocumentSet graph with built-in document nodes/edges for OMG UML 2.5
   *
   * @param documentURIMapper OASIS XML Catalog-based mapping of package or element URIs
   *                          to serializable document URLs and element URIs
   * @param builtInURIMapper OASIS XML Catalog-based mapping of package or element URIs
   *                         to OMG-published document URLs and element URIs
   * @param nodeT Scala type information about Document[UML] graph nodes
   * @param edgeT Scala type information about Document[UML] to Document[UML] graph edges
   * @return A DocumentSet graph
   */
  def initializeDocumentSet
  ( documentURIMapper: CatalogURIMapper,
    builtInURIMapper: CatalogURIMapper )
  ( implicit
    nodeT: TypeTag[Document[Uml]],
    edgeT: TypeTag[DocumentEdge[Document[Uml]]] )
  : NonEmptyList[java.lang.Throwable] \&/ DocumentSet[Uml]

}