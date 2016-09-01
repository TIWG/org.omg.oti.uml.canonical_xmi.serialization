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

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.read.api.UML
import org.omg.oti.uml.read.operations.UMLOps
import org.omg.oti.uml.write.api.{UMLFactory, UMLUpdate}
import org.omg.oti.uml.xmi._

import scala.collection.immutable._
import scala.reflect.runtime.universe._
import scalaz._, Scalaz._

object OTIAdapter {

  def initialize
  [Uml <: UML,
   Uo <: UMLOps[Uml],
   Ch <: OTICharacteristicsProvider[Uml],
   Uf <: UMLFactory[Uml],
   Uu <: UMLUpdate[Uml]]
  (umlOpsCreator: => Set[java.lang.Throwable] \/  Uo,
   otiCharacteristicsCreator: Uo => Set[java.lang.Throwable] \/ Ch,
   factoryCreator: Uo => Set[java.lang.Throwable] \/ Uf,
   updateCreator: Uo => Set[java.lang.Throwable] \/ Uu)
  : Set[java.lang.Throwable] \/ OTIAdapter[Uml, Uo, Ch, Uf, Uu]
  = for {
    umlOps <- umlOpsCreator
    otiCharacteristicsProvider <- otiCharacteristicsCreator(umlOps)
    umlF <- factoryCreator(umlOps)
    umlU <- updateCreator(umlOps)
  } yield OTIAdapter(umlOps, otiCharacteristicsProvider, umlF, umlU)

}

case class OTIAdapter
[Uml <: UML,
 Uo <: UMLOps[Uml],
 Ch <: OTICharacteristicsProvider[Uml],
 Uf <: UMLFactory[Uml],
 Uu <: UMLUpdate[Uml]]
(umlOps: Uo,
 otiCharacteristicsProvider: Ch,
 umlF: Uf,
 umlU: Uu) {

  def withInitialDocumentSet
  [Do <: DocumentOps[Uml],
   Ds <: DocumentSet[Uml]]
  (documentOpsCreator: OTIAdapter[Uml, Uo, Ch, Uf, Uu] => Set[java.lang.Throwable] \/ Do,
   documentSetInitializer: Do => Set[java.lang.Throwable] \&/ Ds)
  ( implicit
    nodeT: TypeTag[Document[Uml]],
    edgeT: TypeTag[DocumentEdge[Document[Uml]]] )
  : Set[java.lang.Throwable] \&/ OTIDocumentSetAdapter[Uml, Uo, Ch, Uf, Uu, Do, Ds]
  = for {
    dOps <- documentOpsCreator(this).toThese
    ds <- documentSetInitializer(dOps)

  } yield
      OTIDocumentSetAdapter(this, dOps, ds)

}