/*
 * Copyright 2014 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Copyright 2015 Airbus.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package org.omg.oti.uml.canonicalXMI

import org.omg.oti.json.common.OTISpecificationRootCharacteristics
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.xmi._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.collection.immutable._
import scala.reflect.runtime.universe._
import scala.Predef.{identity,String}
import scala.StringContext
import scalaz._, Scalaz._

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
  : Set[java.lang.Throwable] \/ URI

  /**
    * Open an input stream on the external document to load
    *
    * @param lurl The `LoadURL` coordinates of the external document to load
    * @return an input stream for reading the XMI contents of the external document to load
    */
  def openExternalDocumentStreamForImport
  (lurl: Uml#LoadURL)
  : Set[java.lang.Throwable] \/ InputStream

  /**
    * Add a set of Documents to a DocumentSet
    *
    * Note that for performance reasons, the return type is `Set[Throwable] \&/ DocumentSet[Uml]` (fast merge)
    * rather than the `Set[Throwable] \&/ DocumentSet[Uml]` (slow merge).
    *
    * @param ds DocumentSet
    * @param d a set of Documents
    * @return The DocumentSet resulting from adding each Document di in d where di can be successfully added.
    */
  def addDocuments
  (ds: DocumentSet[Uml],
   d: Set[_ <: Document[Uml]])
  : Set[java.lang.Throwable] \&/ DocumentSet[Uml] = {

    val r0: Set[java.lang.Throwable] \&/ DocumentSet[Uml] = \&/.That(ds)
    val rN: Set[java.lang.Throwable] \&/ DocumentSet[Uml] = (r0 /: d) { case (acc, di) =>

      val acci
      : Set[java.lang.Throwable] \&/ DocumentSet[Uml]
      = acc.flatMap { dsi =>
        val added1
        : Set[java.lang.Throwable] \/ DocumentSet[Uml]
        = addDocument(dsi, di)

        val added2
        : Set[java.lang.Throwable] \&/ DocumentSet[Uml]
        = added1
          .fold[Set[java.lang.Throwable] \&/ DocumentSet[Uml]](
          l = (a: Set[java.lang.Throwable]) => {
            val next
            : Set[java.lang.Throwable] \&/ DocumentSet[Uml]
            = acc.bimap[Set[java.lang.Throwable], DocumentSet[Uml]](
              f = (as: Set[java.lang.Throwable]) => {
                as ++ a
              },
              g = identity
            )
            next
          },
          r = (b: DocumentSet[Uml]) => {
            val next
            : Set[java.lang.Throwable] \&/ DocumentSet[Uml]
            = acc.onlyThis.fold[Set[java.lang.Throwable] \&/ DocumentSet[Uml]](
              \&/.That(b)
            ) {
              nels =>
                \&/.Both(nels, b)
            }
            next
          }
        )

        added2
      }
      acci
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
  : Set[java.lang.Throwable] \/ DocumentSet[Uml]

  def freezeMutableDocument
  (ds: DocumentSet[Uml],
   d: MutableDocument[Uml])
  : Set[java.lang.Throwable] \/ (Document[Uml], DocumentSet[Uml]) =
    d match {
    case mD: BuiltInMutableDocument[Uml] =>
      if (!ds.builtInMutableDocuments.contains(mD))
        -\/(Set(UMLError.umlAdaptationError(
            s"freezeMutableDocument: mutable document, ${mD.info} is not in the document set!")))
      else 
        ds.freezeBuiltInMutableDocument(mD)
    case mD: SerializableMutableDocument[Uml] =>
      if (!ds.serializableMutableDocuments.contains(mD))
        -\/(Set(UMLError.umlAdaptationError(
            s"freezeMutableDocument: mutable document, ${mD.info} is not in the document set!")))
      else 
        ds.freezeSerializableMutableDocument(mD)
  }

  /**
    * Create a BuiltInMutableDocument for a root package scope whose contents are subject to change.
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
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
  : Set[java.lang.Throwable] \/ ( BuiltInImmutableDocument[Uml], DocumentSet[Uml] )

  /**
    * Create a BuiltInImmutableDocument for a root package scope whose contents are fully known.
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
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
  : Set[java.lang.Throwable] \/ ( BuiltInMutableDocument[Uml], DocumentSet[Uml] )

  /**
    * Create a LoadingMutableDocument for a root package scope whose contents are subject to change
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
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
  : Set[java.lang.Throwable] \/ ( LoadingMutableDocument[Uml], DocumentSet[Uml] )
  
  /**
    * Create a SerializableImmutableDocument for an existing root package whose contents are fully known
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Creation
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
  : Set[java.lang.Throwable] \/ ( SerializableImmutableDocument[Uml], DocumentSet[Uml] )

  /**
    * Create a SerializableMutableDocument for a root package scope whose contents are subject to change
    *
    * @see OMG XMI 2.5.1, formal/2015-06-07, section 7.13.2 Procedures, Document Import
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
  : Set[java.lang.Throwable] \/ ( SerializableMutableDocument[Uml], DocumentSet[Uml] )

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
  : Set[java.lang.Throwable] \&/ DocumentSet[Uml]

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
  : Set[java.lang.Throwable] \&/ DocumentSet[Uml]

}