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

package org.omg.oti.uml

import org.omg.oti.uml.read.api.{UMLElement, UML}
import org.omg.oti.uml.xmi.IDGenerator

import scala.collection.immutable.{Iterable,Set}
import scala.Predef.String
import scalaz._, Scalaz._

/**
 * = Generic OTI-based facilities for OMG XMI Documents and OMG MOF Extents =
 *
 *   - [[org.omg.oti.uml.canonicalXMI `DocumentSet`]]: A graph of OMG XMI Documents (nodes)
 *     and cross-references (edges)
 *   - [[org.omg.oti.uml.canonicalXMI `DocumentOps`]]: API for OMG XMI Document production (serialization)
 */
package object canonicalXMI {

  def catalogURIMapperException
  (message: String,
   cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  : java.lang.Throwable =
    new CatalogURIMapperException(message, cause)

  def catalogURIMapperException
  (message: String,
   cause: java.lang.Throwable)
  : java.lang.Throwable =
    new CatalogURIMapperException(message, Set(cause).some)

  def resolvedDocumentSetException[Uml <: UML]
  (rds: ResolvedDocumentSet[Uml],
   message: String,
   cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  : java.lang.Throwable =
    new ResolvedDocumentSetException(rds, message, cause)

  def resolvedDocumentSetException[Uml <: UML]
  (rds: ResolvedDocumentSet[Uml],
   message: String,
   cause: java.lang.Throwable)
  : java.lang.Throwable =
    new ResolvedDocumentSetException(rds, message, Set(cause).some)

  def documentIDGeneratorException[Uml <: UML]
  (idGenerator: IDGenerator[Uml],
   elements: Iterable[UMLElement[Uml]],
   message: String,
   cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  : java.lang.Throwable =
    new UMLError.IDGeneratorException(idGenerator, elements, message, cause)

  def documentIDGeneratorException[Uml <: UML]
  (idGenerator: IDGenerator[Uml],
   elements: Iterable[UMLElement[Uml]],
   message: String,
   cause: java.lang.Throwable)
  : java.lang.Throwable =
    new UMLError.IDGeneratorException(idGenerator, elements, message, Set(cause).some)

  def documentUUIDGeneratorException[Uml <: UML]
  (idGenerator: IDGenerator[Uml],
   elements: Iterable[UMLElement[Uml]],
   message: String,
   cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  : java.lang.Throwable =
    new UMLError.UUIDGeneratorException(idGenerator, elements, message, cause)

  def documentUUIDGeneratorException[Uml <: UML]
  (idGenerator: IDGenerator[Uml],
   elements: Iterable[UMLElement[Uml]],
   message: String,
   cause: java.lang.Throwable)
  : java.lang.Throwable =
    new UMLError.UUIDGeneratorException(idGenerator, elements, message, Set(cause).some)

  def documentOpsException[Uml <: UML]
  ( dOps: DocumentOps[Uml],
    message: String,
    cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  : java.lang.Throwable =
    new DocumentOpsException(dOps, message, cause)

  def documentOpsException[Uml <: UML]
  ( dOps: DocumentOps[Uml],
    message: String,
    cause: java.lang.Throwable)
  : java.lang.Throwable =
    new DocumentOpsException(dOps, message, Set(cause).some)

}