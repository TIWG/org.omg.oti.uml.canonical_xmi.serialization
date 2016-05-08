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
package org.omg.oti.uml.canonicalXMI.helper

import org.omg.oti.uml.canonicalXMI.{DocumentOps, DocumentSet, ResolvedDocumentSet, UnresolvedElementCrossReference}
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.read.api.UML
import org.omg.oti.uml.read.operations.UMLOps
import org.omg.oti.uml.write.api.{UMLFactory, UMLUpdate}
import org.omg.oti.uml.xmi._

import scala.collection.immutable._
import scalaz._

case class OTIResolvedDocumentSetAdapter
[Uml <: UML,
 Uo <: UMLOps[Uml],
 Ch <: OTICharacteristicsProvider[Uml],
 Uf <: UMLFactory[Uml],
 Uu <: UMLUpdate[Uml],
 Do <: DocumentOps[Uml],
 Ds <: DocumentSet[Uml]]
(otiAdapter: OTIAdapter[Uml, Uo, Ch, Uf, Uu],
 documentOps: Do,
 rds: ResolvedDocumentSet[Uml],
 unresolvedElementCrossReferences: Vector[UnresolvedElementCrossReference[Uml]]) {

  def withIDGenerator[Ig <: IDGenerator[Uml]]
  (generatorCreator: DocumentSet[Uml] => Set[java.lang.Throwable] \/ Ig)
  : Set[java.lang.Throwable] \/ OTIResolvedDocumentSetGeneratorAdapter[Uml, Uo, Ch, Uf, Uu, Do, Ds, Ig]
  = for {
    idg <- generatorCreator(rds.ds)
  } yield OTIResolvedDocumentSetGeneratorAdapter(otiAdapter, documentOps, rds, idg)
}