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

import java.lang.System
import scala.{Int, StringContext, Unit}
import scala.Predef.String
import scalaz._

/**
  * Quantity Kinds for the telemetry of the OTI Document resolution process
  */
object DocumentResolverProgressTelemetry {

  /**
    * String @@ DocumentURL
    */
  sealed trait DocumentURL
  val DocumentURL = Tag.of[DocumentURL]

  /**
    * Int @@ NumberOfDocuments
    */
  sealed trait NumberOfDocuments
  val NumberOfDocuments = Tag.of[NumberOfDocuments]

  /**
    * Int @@ NumberOfElements
    */
  sealed trait NumberOfElements
  val NumberOfElements = Tag.of[NumberOfElements]

  /**
    * Int @@ NumberOfTriples
    */
  sealed trait NumberOfTriples
  val NumberOfTriples = Tag.of[NumberOfTriples]

  /**
    * Int @@ NumberOfHyperEdges
    */
  sealed trait NumberOfHyperEdges
  val NumberOfHyperEdges = Tag.of[NumberOfHyperEdges]

  /**
    * Int @@ NumberOfUnresolvedCrossReferences
    */
  sealed trait NumberOfUnresolvedCrossReferences
  val NumberOfUnresolvedCrossReferences = Tag.of[NumberOfUnresolvedCrossReferences]

  /**
    * String @@ Duration
    */
  sealed trait Duration
  val Duration = Tag.of[Duration]

  type ScanStarted =
  Int @@ NumberOfDocuments => Unit

  type ScanDocumentStarted =
  String @@ DocumentURL => Unit

  type ScanDocumentEnded =
  (Int @@ NumberOfElements, String @@ Duration) => Unit

  type ScanEnded =
  (Int @@ NumberOfDocuments, Int @@ NumberOfElements, String @@ Duration) => Unit

  type ResolveStarted =
  Int @@ NumberOfDocuments => Unit

  type ResolveDocumentStarted =
  (String @@ DocumentURL, Int @@ NumberOfElements) => Unit

  type ResolveDocumentStepped =
  (Int @@ NumberOfElements, Int @@ NumberOfElements, Int @@ NumberOfTriples, String @@ Duration) => Unit

  type ResolveDocumentEnded =
  (Int @@ NumberOfElements, Int @@ NumberOfTriples, String @@ Duration) => Unit

  type ResolveEnded =
  (Int @@ NumberOfDocuments,
    Int @@ NumberOfElements, Int @@ NumberOfTriples,
    Int @@ NumberOfHyperEdges, Int @@ NumberOfUnresolvedCrossReferences,
    String @@ Duration) => Unit


  def scanStarted
  (nd: Int @@ NumberOfDocuments)
  : Unit
  = System.out.println(s"# Begin scanning ${NumberOfDocuments.unwrap(nd)} documents...")

  def scanDocumentStarted
  (url: String @@ DocumentURL)
  : Unit
  = System.out.println(s"# Begin scanning document: $url")

  def scanDocumentEnded
  (ne: Int @@ NumberOfElements, d: String @@ Duration)
  : Unit
  = System.out.println(s"# => Scanned $ne elements in $d")

  def scanEnded
  (nd: Int @@ NumberOfDocuments,
   ne: Int @@ NumberOfElements,
   d: String @@ Duration)
  : Unit
  = System.out.println(s"\n# Found $ne elements scanning all $nd documents.")

  def resolveStarted
  (nd: Int @@ NumberOfDocuments)
  : Unit
  = System.out.println(s"\n# Begin resolving $nd documents...")

  def resolveDocumentStarted
  (url: String @@ DocumentURL,
   ne: Int @@ NumberOfElements)
  : Unit
  = System.out.println(s"# Begin resolving $ne elements in document $url")

  def resolveDocumentStepped
  (ne: Int @@ NumberOfElements,
   neTotal: Int @@ NumberOfElements,
   nt: Int @@ NumberOfTriples,
   d: String @@ Duration)
  : Unit
  = System.out.println(s"# $ne / $neTotal elements, $nt total triples => $d")

  def resolveDocumentEnded
  (ne: Int @@ NumberOfElements,
   nt: Int @@ NumberOfTriples,
   d: String @@ Duration)
  : Unit
  = System.out.println(s"# Finished resolving $ne document elements, cummulative triple count: $nt in $d")

  def resolveEnded
  (nd: Int @@ NumberOfDocuments,
   ne: Int @@ NumberOfElements,
   nt: Int @@ NumberOfTriples,
   nh: Int @@ NumberOfHyperEdges,
   nu: Int @@ NumberOfUnresolvedCrossReferences,
   d: String @@ Duration)
  : Unit
  = System.out.println(
    s"# Created $nh hyper edges ($nt triples) among $nd document hyper nodes ($ne elements) "+
    s"with $nu unresolved cross-reference triples")


  val printTelemetry =
    DocumentResolverProgressTelemetry(
      scanStarted _,
      scanDocumentStarted _,
      scanDocumentEnded _,
      scanEnded _,
      resolveStarted _,
      resolveDocumentStarted _,
      resolveDocumentStepped _,
      resolveDocumentEnded _,
      resolveEnded _)

}

case class DocumentResolverProgressTelemetry
(scanStarted: DocumentResolverProgressTelemetry.ScanStarted,
 scanDocumentStarted: DocumentResolverProgressTelemetry.ScanDocumentStarted,
 scanDocumentEnded: DocumentResolverProgressTelemetry.ScanDocumentEnded,
 scanEnded: DocumentResolverProgressTelemetry.ScanEnded,
 resolveStarted: DocumentResolverProgressTelemetry.ResolveStarted,
 resolveDocumentStarted: DocumentResolverProgressTelemetry.ResolveDocumentStarted,
 resolveDocumentStepped: DocumentResolverProgressTelemetry.ResolveDocumentStepped,
 resolveDocumentEnded: DocumentResolverProgressTelemetry.ResolveDocumentEnded,
 resolveEnded: DocumentResolverProgressTelemetry.ResolveEnded)