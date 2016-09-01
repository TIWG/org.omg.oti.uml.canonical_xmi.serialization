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

import org.omg.oti.uml.canonicalXMI.{DocumentOps, DocumentSet, ResolvedDocumentSet}
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.read.api.UML
import org.omg.oti.uml.read.operations.UMLOps
import org.omg.oti.uml.write.api.{UMLFactory, UMLUpdate}
import org.omg.oti.uml.xmi.IDGenerator

case class OTIResolvedDocumentSetGeneratorAdapter
[Uml <: UML,
 Uo <: UMLOps[Uml],
 Ch <: OTICharacteristicsProvider[Uml],
 Uf <: UMLFactory[Uml],
 Uu <: UMLUpdate[Uml],
 Do <: DocumentOps[Uml],
 Ds <: DocumentSet[Uml],
 Ig <: IDGenerator[Uml]]
(otiAdapter: OTIAdapter[Uml, Uo, Ch, Uf, Uu],
 documentOps: Do,
 rds: ResolvedDocumentSet[Uml],
 idg: Ig) {

  implicit val umlOps: Uo = otiAdapter.umlOps
}