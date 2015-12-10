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
package org.omg.oti.uml

import org.omg.oti.uml.read.api.{UMLElement, UML}
import org.omg.oti.uml.xmi.IDGenerator

import scala.collection.immutable.Iterable
import scala.{Option,None}
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
    new CatalogURIMapperException(message, cause.wrapNel.some)

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
    new ResolvedDocumentSetException(rds, message, cause.wrapNel.some)

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
    new UMLError.IDGeneratorException(idGenerator, elements, message, cause.wrapNel.some)

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
    new DocumentOpsException(dOps, message, cause.wrapNel.some)

}