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

package org.omg.oti.uml.canonicalXMI.helper

import org.omg.oti.json.common.OTISpecificationRootCharacteristics
import org.omg.oti.uml.RelationTriple
import org.omg.oti.uml.canonicalXMI.{DocumentOps, DocumentResolverProgressTelemetry, DocumentSet}
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.read.api.{UML, UMLElement, UMLPackage}
import org.omg.oti.uml.read.operations.UMLOps
import org.omg.oti.uml.write.api.{UMLFactory, UMLUpdate}
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.{Boolean, Option}
import scalaz._

case class OTIDocumentSetAdapter
[Uml <: UML,
 Uo <: UMLOps[Uml],
 Ch <: OTICharacteristicsProvider[Uml],
 Uf <: UMLFactory[Uml],
 Uu <: UMLUpdate[Uml],
 Do <: DocumentOps[Uml],
 Ds <: DocumentSet[Uml]]
(otiAdapter: OTIAdapter[Uml, Uo, Ch, Uf, Uu],
 documentOps: Do,
 ds: Ds) {

  def getSpecificationRootCharacteristics
  (self: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Option[OTISpecificationRootCharacteristics]
  = {
    val result
    : Set[java.lang.Throwable] \/ Option[OTISpecificationRootCharacteristics]
    = otiAdapter.otiCharacteristicsProvider.getSpecificationRootCharacteristics(self)

    result.map { oc =>
      oc orElse
        ds.lookupDocumentByScope(self)
          .map(_.info)
    }
  }

  def addDocuments[D <: Document[Uml]]
  (documents: Set[D])
  : Set[java.lang.Throwable] \&/ DocumentSet[Uml]
  = documentOps
    .addDocuments(ds, documents.filter(d => !ds.allDocuments.map(_.scope).contains(d.scope)))

  def resolve
  (ignoreCrossReferencedElementFilter: UMLElement[Uml] => Boolean,
   unresolvedElementMapper: UMLElement[Uml] => Option[UMLElement[Uml]],
   includeAllForwardRelationTriple: (Document[Uml], RelationTriple[Uml], Document[Uml]) => Boolean,
   progressTelemetry: DocumentResolverProgressTelemetry)
  : Set[java.lang.Throwable] \&/ OTIResolvedDocumentSetAdapter[Uml, Uo, Ch, Uf, Uu, Do, Ds]
  = ds
    .resolve(ignoreCrossReferencedElementFilter, unresolvedElementMapper, includeAllForwardRelationTriple, progressTelemetry)
    .map { case (rds, unresolved) =>
      OTIResolvedDocumentSetAdapter(otiAdapter, documentOps, rds, unresolved)
    }

}