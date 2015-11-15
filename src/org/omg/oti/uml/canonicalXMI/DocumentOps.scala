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

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.characteristics.{OTISpecificationRootCharacteristics, OTICharacteristicsProvider}
import org.omg.oti.uml.xmi._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.Predef.String
import scala.collection.immutable._
import scala.reflect.runtime.universe._
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
   * Create a SerializableDocument for a root package scope created as part of a document import process
   *
   * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
   *
   * @param info the OTI specification characteristics of the `scope` UML Package as the root of an OTI document
   * @param documentURL the `LoadURL` information about the external URL from where
   *                    the serializable document contents will be read into the contents of
   *                    the root package
   * @param root the root package scope of the OTI serializable document
   * @return If successful, a SerializableDocument for the `root` package scope
   */
   def createSerializableDocumentFromImportedRootPackage
   (info: OTISpecificationRootCharacteristics,
    documentURL: Uml#LoadURL,
    root: UMLPackage[Uml])
   (implicit ds: DocumentSet[Uml])
   : NonEmptyList[java.lang.Throwable] \/ SerializableDocument[Uml]

  /**
   * Create a BuiltInDocument for a root package scope that is part of a tool-specific implementation
   * of OMG UML 2.5, a special case of a document import process where the imported document, `root`
   * is built in the specific tool used.
   *
   * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
   *
   * @param info the OTI specification characteristics of the `scope` UML Package as the root of an OTI document
   * @param documentURL the `LoadURL` information about the external URL from where
   *                    the contents of built-in document contents correspond to those of the root package
   * @param root a tool-specific root package corresponding to the tool-specific implementation
   *             of an OMG-defined document (e.g., the OMG UML2.5 PrimitiveTypes library)
   * @return A BuiltInDocument if the `root` package is recognized as the root package scope of
   *         a tool-specific built-in document corresponding to a known OMG published document.
   */
   def createBuiltInDocumentFromBuiltInRootPackage
   (info: OTISpecificationRootCharacteristics,
    documentURL: Uml#LoadURL,
    root: UMLPackage[Uml])
   : NonEmptyList[java.lang.Throwable] \/ BuiltInDocument[Uml]

  /**
   * Create a SerializableDocument for an existing root package
   *
   * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Creation
   *
   * @param info the OTI specification characteristics of the `scope` UML Package as the root of an OTI document
   * @param root The root package scope of the serializable document
   * @param specificationRootPackages The map of root packages to their characteristics
   * @return A SerializableDocument if the `root` package has the necessary information to specify
   *         how it should be eventually serialized per OMG XMI 2.5.1
   */
   def createSerializableDocumentFromExistingRootPackage
   (info: OTISpecificationRootCharacteristics,
    root: UMLPackage[Uml],
    specificationRootPackages: Map[UMLPackage[Uml], OTISpecificationRootCharacteristics])
   : NonEmptyList[java.lang.Throwable] \/ SerializableDocument[Uml]

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
  : NonEmptyList[java.lang.Throwable] \/ DocumentSet[Uml]

  /**
   * Create a DocumentSet graph for document nodes (serializable or built-in) and inter-document edges
   *
   * @param serializableDocuments The set of SerializableDocument nodes in the graph
   * @param builtInDocuments The set of BuiltInDocument nodes in the graph
   * @param builtInDocumentEdges The set of inter-document edges amongst built-in document nodes
   * @param documentURIMapper OASIS XML Catalog-based mapping of package or element URIs
   *                          to serializable document URLs and element URIs
   * @param builtInURIMapper OASIS XML Catalog-based mapping of package or element URIs
   *                         to OMG-published document URLs and element URIs
   * @param ops OTI UML Read Operations API
   * @param nodeT Scala type information about Document[UML] graph nodes
   * @param edgeT Scala type information about Document[UML] to Document[UML] graph edges
   * @return A DocumentSet graph
   */
  def createDocumentSet
  ( serializableDocuments: Set[SerializableDocument[Uml]],
    builtInDocuments: Set[BuiltInDocument[Uml]],
    builtInDocumentEdges: Set[DocumentEdge[Document[Uml]]],
    documentURIMapper: CatalogURIMapper,
    builtInURIMapper: CatalogURIMapper,
    aggregate: Uml#DocumentSetAggregate)
  ( implicit
    ops: UMLOps[Uml],
    nodeT: TypeTag[Document[Uml]],
    edgeT: TypeTag[DocumentEdge[Document[Uml]]] )
  : NonEmptyList[java.lang.Throwable] \/ DocumentSet[Uml]
   
  /**
   * Add a serializable document as a new node to an existing document set graph
   *
   * @param ds A DocumentSet graph
   * @param d A SerializableDocument to add as a new node to `ds`
   * @return A new DocumentSet, ds', whose nodes should be ds'=d + ds.serializableDocument
   */
  def addDocument
  (ds: DocumentSet[Uml],
   d: SerializableDocument[Uml])
  : NonEmptyList[java.lang.Throwable] \/ DocumentSet[Uml]

}